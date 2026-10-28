(ns clojure-algorithms.web-programming.reddit
  "Reddit subreddit data via old JSON endpoints.

  STUB: fetching needs network access. The valid-field set, term
  validation and URL builder are ported as pure logic.")

(def valid-terms
  #{"approved_at_utc" "approved_by" "author_flair_background_color"
    "author_flair_css_class" "author_flair_richtext"
    "author_flair_template_id" "author_fullname" "author_premium"
    "can_mod_post" "category" "clicked" "content_categories" "created_utc"
    "downs" "edited" "gilded" "gildings" "hidden" "hide_score"
    "is_created_from_ads_ui" "is_meta" "is_original_content"
    "is_reddit_media_domain" "is_video" "link_flair_css_class"
    "link_flair_richtext" "link_flair_text" "link_flair_text_color"
    "media_embed" "mod_reason_title" "name" "permalink" "pwls" "quarantine"
    "saved" "score" "secure_media" "secure_media_embed" "selftext"
    "subreddit" "subreddit_name_prefixed" "subreddit_type" "thumbnail"
    "title" "top_awarded_type" "total_awards_received" "ups" "upvote_ratio"
    "url" "user_reports"})

(defn subreddit-url
  [subreddit age limit]
  (str "https://www.reddit.com/r/" subreddit "/" age ".json?limit=" limit))

(defn validate-terms
  "Returns nil when every term in `wanted` is valid; otherwise throws
  IllegalArgumentException listing the invalid ones (sorted)."
  [wanted]
  (let [invalid (sort (remove valid-terms wanted))]
    (when (seq invalid)
      (throw (IllegalArgumentException.
              (str "Invalid search term: "
                   (clojure.string/join ", " invalid)))))))

(defn get-subreddit-data
  "STUB: would return post data for `subreddit`."
  ([subreddit] (get-subreddit-data subreddit 1 "new" []))
  ([subreddit limit age wanted-data]
   (validate-terms (or wanted-data []))
   (throw (ex-info "Network access required: GET reddit.com"
                   {:reason :network-required
                    :url (subreddit-url subreddit age limit)}))))
