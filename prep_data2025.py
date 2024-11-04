# %% [code]
"""
Data Preparation Module for NFL Big Data Bowl 2025

This module processes raw NFL tracking data to prepare it for machine learning models.
It includes functions for loading, cleaning, and transforming the data, as well as
splitting it into train, validation, and test sets.

Functions:
    get_players_df: Load and preprocess player data
    get_plays_df: Load and preprocess play data
    get_tracking_df: Load and preprocess tracking data
    add_features_to_tracking_df: Add derived features to tracking data
    convert_tracking_to_cartesian: Convert polar coordinates to Cartesian
    standardize_tracking_directions: Standardize play directions
    augment_mirror_tracking: Augment data by mirroring the field
    add_relative_positions: Add relative position features
    defFormation_df: Generate target dataframe defFormation prediction
    split_train_test_val: Split data into train, validation, and test sets
    main: Main execution function

"""

from argparse import ArgumentParser
from pathlib import Path

import polars as pl

INPUT_DATA_DIR = Path(r'D:\NFL\BDB25\BDB25')
OUT_DIR = Path(r'D:\NFL\BDB25\BDB25')


def get_players_df() -> pl.DataFrame:
    """
    Load player-level data and preprocesses features.

    Returns:
        pl.DataFrame: Preprocessed player data with additional features.
    """
    return (
        pl.read_csv(INPUT_DATA_DIR / "players.csv", null_values=["NA", "nan", "N/A", "NaN", ""])
        .with_columns(
            height_inches=(
                pl.col("height").str.split("-").map_elements(lambda s: int(s[0]) * 12 + int(s[1]), return_dtype=int)
            )
        )
        .with_columns(
            weight_Z=(pl.col("weight") - pl.col("weight").mean()) / pl.col("weight").std(),
            height_Z=(pl.col("height_inches") - pl.col("height_inches").mean()) / pl.col("height_inches").std(),
        )
    )


def get_plays_df() -> pl.DataFrame:
    """
    Load play-level data and preprocesses features.

    Returns:
        pl.DataFrame: Preprocessed play data with additional features.
    """
    return pl.read_csv(INPUT_DATA_DIR / "plays.csv", null_values=["NA", "nan", "N/A", "NaN", ""]).with_columns(
        distanceToGoal=(
            pl.when(pl.col("possessionTeam") == pl.col("yardlineSide"))
            .then(100 - pl.col("yardlineNumber"))
            .otherwise(pl.col("yardlineNumber"))
        )
    )


def get_tracking_df() -> pl.DataFrame:
    """
    Load tracking data and preprocesses features. Notably, exclude rows representing the football's movement.

    Returns:
        pl.DataFrame: Preprocessed tracking data with additional features.
    """
    # don't include football rows for this project.  
    # NOTE: Only processing week 1 for the sake of time.  Change "1" to "*" to process all weeks
    return pl.read_csv(INPUT_DATA_DIR / "tracking_week_*.csv", null_values=["NA", "nan", "N/A", "NaN", ""]).filter(
        (pl.col("club")!= "football")#pl.col("team") != "football"
    )


def add_features_to_tracking_df(
    tracking_df: pl.DataFrame,
    players_df: pl.DataFrame,
    plays_df: pl.DataFrame,
) -> pl.DataFrame:
    """
    Consolidates play and player level data into the tracking data.

    Args:
        tracking_df (pl.DataFrame): Tracking data
        players_df (pl.DataFrame): Player data
        plays_df (pl.DataFrame): Play data

    Returns:
        pl.DataFrame: Tracking data with additional features.
    """
    # add `is_ball_carrier`, `team_indicator`, and other features to tracking data
    og_len = len(tracking_df)
    tracking_df = (
        tracking_df.join(
            plays_df.select(
                "gameId",
                "playId",
                "possessionTeam",
                "down",
                "yardsToGo",
            ),
            on=["gameId", "playId"],
            how="inner",
        )
        #.join(
        #    players_df.select(["nflId", "weight_Z", "height_Z"]).unique(),
        #    on="nflId",
        #    how="inner",
        #)
        .with_columns(
            side=pl.when(pl.col("club") == pl.col("possessionTeam"))
            .then(pl.lit(1))
            .otherwise(pl.lit(-1))
            .alias("side"),
        ).drop(["possessionTeam"])
    )
    #tracking_df = tracking_df.filter(
    #    pl.col("club") !=pl.col("possessionTeam")).drop(["possessionTeam"])
    #assert len(tracking_df) == og_len, "Lost rows when joining tracking data with play/player data"

    return tracking_df


def convert_tracking_to_cartesian(tracking_df: pl.DataFrame) -> pl.DataFrame:
    """
    Convert polar coordinates to Unit-circle Cartesian format.

    Args:
        tracking_df (pl.DataFrame): Tracking data

    Returns:
        pl.DataFrame: Tracking data with Cartesian coordinates.
    """
    return (
        tracking_df.with_columns(
            dir=((pl.col("dir") - 90) * -1) % 360,
            o=((pl.col("o") - 90) * -1) % 360,
        )
        # convert polar vectors to cartesian ((s, dir) -> (vx, vy), (o) -> (ox, oy))
        .with_columns(
            vx=pl.col("s") * pl.col("dir").radians().cos(),
            vy=pl.col("s") * pl.col("dir").radians().sin(),
            ox=pl.col("o").radians().cos(),
            oy=pl.col("o").radians().sin(),
        )
    )


def standardize_tracking_directions(tracking_df: pl.DataFrame) -> pl.DataFrame:
    """
    Standardize play directions to always moving left to right.

    Args:
        tracking_df (pl.DataFrame): Tracking data

    Returns:
        pl.DataFrame: Tracking data with standardized directions.
    """
    return tracking_df.with_columns(
        x=pl.when(pl.col("playDirection") == "right").then(pl.col("x")).otherwise(120 - pl.col("x")),
        y=pl.when(pl.col("playDirection") == "right").then(pl.col("y")).otherwise(53.3 - pl.col("y")),
        vx=pl.when(pl.col("playDirection") == "right").then(pl.col("vx")).otherwise(-1 * pl.col("vx")),
        vy=pl.when(pl.col("playDirection") == "right").then(pl.col("vy")).otherwise(-1 * pl.col("vy")),
        ox=pl.when(pl.col("playDirection") == "right").then(pl.col("ox")).otherwise(-1 * pl.col("ox")),
        oy=pl.when(pl.col("playDirection") == "right").then(pl.col("oy")).otherwise(-1 * pl.col("oy")),
    ).drop("playDirection")


def augment_mirror_tracking(tracking_df: pl.DataFrame) -> pl.DataFrame:
    """
    Augment data by mirroring the field assuming all plays are moving right.
    There are arguments to not do this as football isn't perfectly symmetric (e.g. most QBs are right-handed) but
    defFormation is mostly symmetrical and for the sake of this demo I think more data is more important.

    Args:
        tracking_df (pl.DataFrame): Tracking data

    Returns:
        pl.DataFrame: Augmented tracking data.
    """
    og_len = len(tracking_df)

    mirrored_tracking_df = tracking_df.clone().with_columns(
        # only flip y values
        y=53.3 - pl.col("y"),
        vy=-1 * pl.col("vy"),
        oy=-1 * pl.col("oy"),
        mirrored=pl.lit(True),
    )

    tracking_df = pl.concat(
        [
            tracking_df.with_columns(mirrored=pl.lit(False)),
            mirrored_tracking_df,
        ],
        how="vertical",
    )

    assert len(tracking_df) == og_len * 2, "Lost rows when mirroring tracking data"
    return tracking_df


def get_defFormation(tracking_df: pl.DataFrame, plays_df: pl.DataFrame) -> pl.DataFrame:
    """
    Generate target dataframe for defFormation prediction.

    Args:
        tracking_df (pl.DataFrame): Tracking data

    Returns:
        tuple: tuple containing defFormation coverage
    """
    
    # drop rows where defFormation is None
    plays_df = plays_df.filter(pl.col("defendersInBox").is_not_null())
    
    tracking_df = tracking_df.join(
        plays_df[["gameId", "playId","defendersInBox"]],
        on=["gameId", "playId"],
        how="inner",
    )
    defFormation_df = (tracking_df[["gameId", "playId", "mirrored", "frameId", "defendersInBox"]]
       .unique()  # Polars equivalent of drop_duplicates()
    )

    tracking_df = tracking_df.drop(["defendersInBox"])
    
    return defFormation_df, tracking_df
    

def split_train_test_val(
    tracking_df: pl.DataFrame, target_df: pl.DataFrame, split: str = "train_test_val"
) -> dict[str, pl.DataFrame]:
    """
    Split data into train, validation, and test sets or use the entire dataset without splitting.
    Split is 70-15-15 for train-test-val respectively unless 'none' is passed as the split argument.

    Args:
        tracking_df (pl.DataFrame): Tracking data.
        target_df (pl.DataFrame): Target data.
        split (str): Specifies split type. Options are "train_test_val" (default) or "none".

    Returns:
        dict: Dictionary containing train, validation, and test dataframes or the full dataset.
    """
    # Sort the data for consistency
    tracking_df = tracking_df.sort(["gameId", "playId", "mirrored", "frameId"])
    target_df = target_df.sort(["gameId", "playId", "mirrored"])

    

    # Default split logic (70-15-15 split at the play level)
    print(
        f"Total set: {tracking_df.n_unique(['gameId', 'playId', 'mirrored'])} plays,",
        f"{tracking_df.n_unique(['gameId', 'playId', 'mirrored', 'frameId'])} frames"
    )
    
    none_val_ids = tracking_df.select(["gameId", "playId","frameId"]).unique(maintain_order=True)
    none_tracking_df = tracking_df#.join(test_val_ids, on=["gameId", "playId"], how="anti")
    none_tgt_df = target_df#.join(test_val_ids, on=["gameId", "playId"], how="anti")
    print(
        f"None set: {none_tracking_df.n_unique(['gameId', 'playId', 'mirrored'])} plays,",
        f"{none_tracking_df.n_unique(['gameId', 'playId', 'mirrored', 'frameId'])} frames",
    )
    return {
        
        "none_features": none_tracking_df,
        "none_targets": none_tgt_df,
    }



def main():
    """
    Main execution function for data preparation.

    This function orchestrates the entire data preparation process, including:
    1. Loading raw data
    2. Adding features and transforming coordinates
    3. Generating target variables
    4. Splitting data into train, validation, and test sets
    5. Saving processed data to parquet files
    """
    print("Load players")
    players_df = get_players_df()
    print("Load plays")
    plays_df = get_plays_df()
    print("Load tracking")
    tracking_df = get_tracking_df()
    print("tracking_df rows:", len(tracking_df))

    print("Add features to tracking")
    tracking_df = add_features_to_tracking_df(tracking_df, players_df, plays_df)
    del players_df
    print("Convert tracking to cartesian")
    tracking_df = convert_tracking_to_cartesian(tracking_df)
    print("Standardize play direction")
    tracking_df = standardize_tracking_directions(tracking_df)
    print("Augment data by mirroring")
    tracking_df = augment_mirror_tracking(tracking_df)

    print("Generate target - defFormation")
    defFormation_df, rel_tracking_df = get_defFormation(tracking_df, plays_df)

    print("Split none")
    split_dfs = split_train_test_val(rel_tracking_df, defFormation_df)

    out_dir = Path(OUT_DIR)
    out_dir.mkdir(exist_ok=True, parents=True)

    for key, df in split_dfs.items():
    # Remove duplicates to ensure unique rows using Polars unique() method
        df_unique = df.unique()

      #  # Sort DataFrame using specified keys
        sort_keys = ["gameId", "playId", "mirrored", "frameId"]
        df_sorted = df_unique.sort(sort_keys)

        # Print debug information
        print(f"Key: {key}, original shape: {df.shape}, unique shape: {df_unique.shape}, sorted shape: {df_sorted.shape}")

        # Write sorted, unique DataFrame to Parquet
        df_sorted.write_parquet(out_dir / f"{key}.parquet")
    #for key, df in split_dfs.items():
    #    sort_keys = ["gameId", "playId", "mirrored", "frameId"]
     #   df.sort(sort_keys).write_parquet(out_dir / f"{key}.parquet")


if __name__ == "__main__":
    main()