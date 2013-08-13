type user = {
  user_created_at: string;
  user_description: string option;
  user_followers_count: int;
  user_friends_count: int;
  user_id: int64;
  user_lang: string;
  user_name: string;
  user_screen_name: string;
  user_statuses_count: int;
  user_url: string option;
  user_utc_offset: int option;
}

type tweet = {
  tweet_created_at: string;
  tweet_favorite_count: int option;
  tweet_id: int64;
  tweet_in_reply_to_status_id: int64 option;
  tweet_in_reply_to_user_id: int64 option;
  tweet_lang: string option;
  tweet_text: string;
  tweet_retweet_count: int;
  tweet_retweeted: bool;
  tweet_user: user;
}

let user_of_json json = let open Yojson.Basic.Util in
  {
    user_created_at = json |> member "created_at" |> to_string;
    user_description = json |> member "description" |> to_string_option;
    user_followers_count = json |> member "followers_count" |> to_int;
    user_friends_count = json |> member "friends_count" |> to_int;
    user_id = json |> member "id_str" |> to_string |> Int64.of_string;
    user_lang = json |> member "lang" |> to_string;
    user_name = json |> member "name" |> to_string;
    user_screen_name = json |> member "screen_name" |> to_string;
    user_statuses_count = json |> member "statuses_count" |> to_int;
    user_url = json |> member "url" |> to_string_option;
    user_utc_offset = json |> member "utc_offset" |> to_int_option;
  }

let tweet_of_json json = let open Yojson.Basic.Util in
  {
    tweet_created_at = json |> member "created_at" |> to_string;
    tweet_favorite_count = json |> member "favorite_count" |> to_int_option;
    tweet_id = json |> member "id_str" |> to_string |> Int64.of_string;
    tweet_in_reply_to_status_id = json |> member "in_reply_to_status_id" |> to_int_option;
    tweet_in_reply_to_user_id = json |> member "in_reply_to_user_id" |> to_int_option;
    tweet_lang = json |> member "lang" |> to_string_option;
    tweet_text = json |> member "text" |> to_string;
    tweet_retweet_count = json |> member "retweet_count" |> to_int;
    tweet_retweeted = json |> member "retweeted" |> to_bool;
    tweet_user = json |> user_of_json;
  }
