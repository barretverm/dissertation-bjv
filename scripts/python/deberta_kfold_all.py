# INSTALLS

!pip install -q transformers sentencepiece protobuf

# CHANGE FOR EACH MODEL

CLASSIFICATION = "communal"                    # "agentic" or "communal"
DIMENSION      = "AM"                         # "AM" or "CM"
MODEL_NAME     = "microsoft/deberta-v3-small"
N_FOLDS        = 5
SCRIPT_VERSION = "v1_kfold"

# IMPORTS, MOUNT DRIVE, PATHS

import os
import random
import numpy as np
import pandas as pd
from scipy.stats import pearsonr
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold, train_test_split
from google.colab import drive
from datasets import Dataset
from transformers import (
    AutoTokenizer,
    AutoModelForSequenceClassification,
    TrainingArguments,
    Trainer,
    EarlyStoppingCallback
)

# mount drive
drive.mount("/content/gdrive", force_remount=True)

# paths
BASE_DIR   = "/content/gdrive/My Drive/Colab Notebooks/main_dissertation_analysis"
DATA_DIR   = f"{BASE_DIR}/data"
OUTPUT_DIR = f"{BASE_DIR}/outputs"

os.makedirs(f"{OUTPUT_DIR}/predictions",     exist_ok=True)
os.makedirs(f"{OUTPUT_DIR}/models",          exist_ok=True)
os.makedirs(f"{OUTPUT_DIR}/checkpoints",     exist_ok=True)
os.makedirs(f"{OUTPUT_DIR}/results_summary", exist_ok=True)

# SET HYPERPARAMETERS

SEED               = 42
MAX_LENGTH         = 512
LABEL_MIN          = 1.0
LABEL_MAX          = 7.0
NUM_EPOCHS         = 8
BATCH_SIZE         = 8
LEARNING_RATE      = 5e-7
WEIGHT_DECAY       = 0.005
WARMUP_STEPS       = 200
#FREEZE_UNTIL_LAYER = 6

# SET SEED FOR REPLICABILITY

random.seed(SEED)
np.random.seed(SEED)

# LOAD AND PREP DATA

df = pd.read_csv(f"{DATA_DIR}/agentic_CM.csv")
print(f"Loaded: communal_AM.csv | shape: {df.shape}")
print(f"Classification values: {df['classification'].unique()}")
print(f"Dimension values: {df['dimension'].unique()}")

# review data
print(df.shape)
print(df.dtypes)
print(df.head())

# descriptives
print(df["BIMI_mean"].describe())

# normalize BIMI labels from 1–7 to 0–1
# this stabilises training; predictions are rescaled back to 1–7 on output
df["label"] = (df["BIMI_mean"] - LABEL_MIN) / (LABEL_MAX - LABEL_MIN)

# keep only the necessary columns
df = df[["ResponseId", "SJTs_all", "SJT_AM", "SJT_CM", "label"]].dropna().reset_index(drop=True)
print(f"\nRows after dropping NaNs: {len(df)}")
print(f"Label range: {df['label'].min():.2f} to {df['label'].max():.2f}")

# TOKENIZER

print(f"\nLoading tokenizer: {MODEL_NAME}")
tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)

def tokenize(texts, labels):
    """
    Tokenizes a list of texts and returns a HuggingFace Dataset
    ready for the Trainer.
    """
    dataset = Dataset.from_dict({
        "text":  [str(t) for t in texts],
        "label": [float(l) for l in labels],
    })

    def tokenize_fn(batch):
        return tokenizer(
            batch["text"],
            max_length=MAX_LENGTH,
            padding="max_length",
            truncation=True,
        )

    dataset = dataset.map(tokenize_fn, batched=True)
    dataset = dataset.remove_columns(["text"])
    dataset.set_format("torch")
    return dataset

# EVALUATION METRICS

def compute_metrics(eval_pred):
    predictions, labels = eval_pred
    predictions = np.array(predictions).squeeze().flatten()
    labels      = np.array(labels).flatten()

    # replace NaNs with midpoint as a safe fallback
    if np.isnan(predictions).any():
        print(f"WARNING: {np.isnan(predictions).sum()} NaN predictions detected")
        predictions = np.where(np.isnan(predictions), 0.5, predictions)

    # if all predictions are identical, pearson is undefined
    if np.all(predictions == predictions[0]):
        return {"pearson_correlation": 0.0, "mse": float("nan"), "rmse": float("nan")}

    pearson_corr = pearsonr(predictions, labels)[0]
    mse          = mean_squared_error(labels, predictions)
    rmse         = np.sqrt(mse)

    return {"pearson_correlation": pearson_corr, "mse": mse, "rmse": rmse}

# SINGLE FOLD TRAINING FUNCTION

def train_fold(train_texts, train_labels, val_texts, val_labels,
               test_texts, test_labels, text_col, fold_num):
    """
    Trains DeBERTa on one fold and returns predictions on the test set.
    """
    print(f"\n  --- Fold {fold_num} | {text_col} ---")

    # tokenize splits for this fold
    tokenized_train = tokenize(train_texts, train_labels)
    tokenized_val   = tokenize(val_texts,   val_labels)
    tokenized_test  = tokenize(test_texts,  test_labels)

    # load a fresh model for each fold — important, avoids leakage between folds
    model = AutoModelForSequenceClassification.from_pretrained(
        MODEL_NAME,
        num_labels=1,               # single output neuron = regression
        ignore_mismatched_sizes=True,
    )

    #  # --- Layer freezing ---
    # for name, param in model.named_parameters():
    #      layer_num = None
    #      if "encoder.layer." in name:
    #          try:
    #              layer_num = int(name.split("encoder.layer.")[1].split(".")[0])
    #          except:
    #              pass
    #      if layer_num is not None:
    #          param.requires_grad = layer_num >= FREEZE_UNTIL_LAYER
    #      else:
    #          param.requires_grad = "classifier" in name

    # # Report trainable vs frozen parameters — verify freezing is correct
    # total_params     = sum(p.numel() for p in model.parameters())
    # trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)
    # frozen_params    = total_params - trainable_params
    # print(f"  Parameters: {total_params:,} total | "
    #   f"{trainable_params:,} trainable | "
    #   f"{frozen_params:,} frozen")

    # checkpoint directory is unique per fold to avoid overwriting
    ckpt_dir = f"{OUTPUT_DIR}/checkpoints/{CLASSIFICATION}_{DIMENSION}_{text_col}_fold{fold_num}_{SCRIPT_VERSION}"

    # training arguments
    training_args = TrainingArguments(
        output_dir                  = ckpt_dir,
        eval_strategy               = "epoch",
        logging_strategy            = "epoch",
        save_strategy               = "epoch",
        num_train_epochs            = NUM_EPOCHS,
        per_device_train_batch_size = BATCH_SIZE,
        learning_rate               = LEARNING_RATE,
        weight_decay                = WEIGHT_DECAY,
        warmup_steps                = WARMUP_STEPS,
        load_best_model_at_end      = True,
        metric_for_best_model       = "eval_loss",
        greater_is_better           = False,
        report_to                   = "none",
        max_grad_norm               = 1.0,
        fp16                        = False,
        seed                        = SEED,
    )

    trainer = Trainer(
        model           = model,
        args            = training_args,
        train_dataset   = tokenized_train,
        eval_dataset    = tokenized_val,
        compute_metrics = compute_metrics,
        callbacks       = [EarlyStoppingCallback(early_stopping_patience=2)],
    )

    trainer.train()

    # generate predictions on this fold's test set
    predictions    = trainer.predict(tokenized_test)
    predicted_norm = predictions.predictions.squeeze()

    # rescale back to 1–7
    predicted_BIMI = predicted_norm * (LABEL_MAX - LABEL_MIN) + LABEL_MIN
    actual_BIMI    = np.array(test_labels) * (LABEL_MAX - LABEL_MIN) + LABEL_MIN

    r, p = pearsonr(actual_BIMI, predicted_BIMI)
    print(f"  Fold {fold_num} Pearson r = {r:.3f} (p = {p:.4f})")

    return predicted_BIMI, actual_BIMI, r, p

# CELL 10 — K-FOLD CROSS-VALIDATION LOOP
# validation set is randomly sampled
# using train_test_split (not taken sequentially from the front of the data)

def run_kfold(df, text_col):
    """
    Runs N_FOLDS-fold cross-validation for a single text column.
    Each fold:
      - KFold splits data into trainval (80%) and test (20%)
      - trainval is further split randomly into train (85%) and val (15%)
    Saves per-fold predictions and a summary CSV.
    Returns all predictions and mean Pearson r.
    """
    print(f"\n{'='*60}")
    print(f"Running {N_FOLDS}-fold CV | {CLASSIFICATION} / {DIMENSION} / {text_col}")
    print(f"{'='*60}")

    kf = KFold(n_splits=N_FOLDS, shuffle=True, random_state=SEED)

    all_predictions = []
    fold_pearsons   = []
    fold_pvalues    = []

    for fold_num, (trainval_idx, test_idx) in enumerate(kf.split(df), start=1):

        trainval_df = df.iloc[trainval_idx].reset_index(drop=True)
        test_df_fold = df.iloc[test_idx].reset_index(drop=True)

        # use train_test_split for a proper random val split ---
        train_df_fold, val_df_fold = train_test_split(
            trainval_df,
            test_size=0.15,
            random_state=SEED,
        )
        train_df_fold = train_df_fold.reset_index(drop=True)
        val_df_fold   = val_df_fold.reset_index(drop=True)

        print(f"\nFold {fold_num}: train={len(train_df_fold)} | val={len(val_df_fold)} | test={len(test_df_fold)}")

        predicted_BIMI, actual_BIMI, r, p = train_fold(
            train_texts  = train_df_fold[text_col].tolist(),
            train_labels = train_df_fold["label"].tolist(),
            val_texts    = val_df_fold[text_col].tolist(),
            val_labels   = val_df_fold["label"].tolist(),
            test_texts   = test_df_fold[text_col].tolist(),
            test_labels  = test_df_fold["label"].tolist(),
            text_col     = text_col,
            fold_num     = fold_num,
        )

        # store fold results with full traceability metadata
        fold_results = test_df_fold[["ResponseId"]].copy()
        fold_results["predicted_BIMI"]  = predicted_BIMI
        fold_results["actual_BIMI"]     = actual_BIMI
        fold_results["fold"]            = fold_num
        fold_results["pearson_r"]       = r
        fold_results["p_value"]         = p
        fold_results["classification"]  = CLASSIFICATION
        fold_results["dimension"]       = DIMENSION
        fold_results["text_input"]      = text_col
        fold_results["model"]           = MODEL_NAME
        fold_results["script_version"]  = SCRIPT_VERSION

        all_predictions.append(fold_results)
        fold_pearsons.append(r)
        fold_pvalues.append(p)

    # aggregate
    all_predictions_df = pd.concat(all_predictions, ignore_index=True)
    mean_r = np.mean(fold_pearsons)
    std_r  = np.std(fold_pearsons)

    print(f"\n{'='*60}")
    print(f"RESULTS: {CLASSIFICATION} / {DIMENSION} / {text_col}")
    print(f"{'='*60}")
    for i, (r, p) in enumerate(zip(fold_pearsons, fold_pvalues), start=1):
        print(f"  Fold {i}: r = {r:.3f} (p = {p:.4f})")
    print(f"\n  Mean Pearson r = {mean_r:.3f} (SD = {std_r:.3f})")
    print(f"{'='*60}")

    # save all fold predictions
    pred_filename = f"predictions_{CLASSIFICATION}_{DIMENSION}_{text_col}_{SCRIPT_VERSION}.csv"
    all_predictions_df.to_csv(f"{OUTPUT_DIR}/predictions/{pred_filename}", index=False)
    print(f"Predictions saved to {OUTPUT_DIR}/predictions/{pred_filename}")

    # save fold-level summary
    summary = pd.DataFrame({
        "fold":           list(range(1, N_FOLDS + 1)),
        "pearson_r":      fold_pearsons,
        "p_value":        fold_pvalues,
        "classification": CLASSIFICATION,
        "dimension":      DIMENSION,
        "text_input":     text_col,
        "model":          MODEL_NAME,
        "script_version": SCRIPT_VERSION,
        "mean_r":         mean_r,
        "std_r":          std_r,
    })
    summary_filename = f"summary_{CLASSIFICATION}_{DIMENSION}_{text_col}_{SCRIPT_VERSION}.csv"
    summary.to_csv(f"{OUTPUT_DIR}/results_summary/{summary_filename}", index=False)
    print(f"Summary saved to {OUTPUT_DIR}/results_summary/{summary_filename}")

    return all_predictions_df, mean_r, std_r, fold_pvalues

# HYPERPARAMETER TUNING CHECK — SINGLE TRAIN/VAL/TEST RUN (70/15/15)
# use this instead of run_kfold() for a quick single run

from sklearn.model_selection import train_test_split

# split data
train_df, temp_df    = train_test_split(df, test_size=0.30, random_state=SEED)
val_df,   test_df    = train_test_split(temp_df, test_size=0.50, random_state=SEED)

train_df = train_df.reset_index(drop=True)
val_df   = val_df.reset_index(drop=True)
test_df  = test_df.reset_index(drop=True)

print(f"Train: {len(train_df)} | Val: {len(val_df)} | Test: {len(test_df)}")

# run for each text column
for text_col in ["SJTs_all", "SJT_AM", "SJT_CM"]:
    predicted_BIMI, actual_BIMI, r, p = train_fold(
        train_texts  = train_df[text_col].tolist(),
        train_labels = train_df["label"].tolist(),
        val_texts    = val_df[text_col].tolist(),
        val_labels   = val_df["label"].tolist(),
        test_texts   = test_df[text_col].tolist(),
        test_labels  = test_df["label"].tolist(),
        text_col     = text_col,
        fold_num     = 1,
    )
    print(f"\n{text_col}: r = {r:.3f} (p = {p:.4f}, n = {len(test_df)})")

# CELL 11 — RUN ALL THREE TEXT COLUMNS

results_all, r_all, sd_all, p_all = run_kfold(df, "SJTs_all")
results_AM,  r_AM,  sd_AM,  p_AM  = run_kfold(df, "SJT_AM")
results_CM,  r_CM,  sd_CM,  p_CM  = run_kfold(df, "SJT_CM")

from scipy.stats import combine_pvalues
import pandas as pd

# load the already-saved summary CSVs
summary_all = pd.read_csv(f"{OUTPUT_DIR}/results_summary/summary_{CLASSIFICATION}_{DIMENSION}_SJTs_all_{SCRIPT_VERSION}.csv")
summary_AM  = pd.read_csv(f"{OUTPUT_DIR}/results_summary/summary_{CLASSIFICATION}_{DIMENSION}_SJT_AM_{SCRIPT_VERSION}.csv")
summary_CM  = pd.read_csv(f"{OUTPUT_DIR}/results_summary/summary_{CLASSIFICATION}_{DIMENSION}_SJT_CM_{SCRIPT_VERSION}.csv")

# compute combined p-values
_, combined_p_all = combine_pvalues(summary_all["p_value"].tolist(), method="stouffer")
_, combined_p_AM  = combine_pvalues(summary_AM["p_value"].tolist(),  method="stouffer")
_, combined_p_CM  = combine_pvalues(summary_CM["p_value"].tolist(),  method="stouffer")

# build and save the final summary table
summary_table = pd.DataFrame({
    "text_input":   ["SJTs_all",    "SJT_AM",      "SJT_CM"],
    "mean_r":       [summary_all["mean_r"].iloc[0], summary_AM["mean_r"].iloc[0], summary_CM["mean_r"].iloc[0]],
    "std_r":        [summary_all["std_r"].iloc[0],  summary_AM["std_r"].iloc[0],  summary_CM["std_r"].iloc[0]],
    "combined_p":   [combined_p_all, combined_p_AM,  combined_p_CM],
    "significance": ["***" if combined_p_all < .001 else "**" if combined_p_all < .01 else "*" if combined_p_all < .05 else "ns",
                     "***" if combined_p_AM  < .001 else "**" if combined_p_AM  < .01 else "*" if combined_p_AM  < .05 else "ns",
                     "***" if combined_p_CM  < .001 else "**" if combined_p_CM  < .01 else "*" if combined_p_CM  < .05 else "ns"],
    "classification": CLASSIFICATION,
    "dimension":      DIMENSION,
    "model":          MODEL_NAME,
    "script_version": SCRIPT_VERSION,
    "n_folds":        N_FOLDS,
    "seed":           SEED,
    "max_length":     MAX_LENGTH,
    "num_epochs":     NUM_EPOCHS,
    "batch_size":     BATCH_SIZE,
    "learning_rate":  LEARNING_RATE,
    "weight_decay":   WEIGHT_DECAY,
    "warmup_steps":   WARMUP_STEPS
})

print("\n===== FINAL SUMMARY =====")
print(summary_table.to_string(index=False))

final_filename = f"final_summary_{CLASSIFICATION}_{DIMENSION}_{SCRIPT_VERSION}.csv"
summary_table.to_csv(f"{OUTPUT_DIR}/results_summary/{final_filename}", index=False)
print(f"\nFinal summary saved to {OUTPUT_DIR}/results_summary/{final_filename}")
