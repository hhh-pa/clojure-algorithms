(ns clojure-algorithms.web-programming.daily-horoscope
  "Daily horoscope scraping from horoscope.com.

  STUB: fetching + HTML parsing need network and an HTML parser. The
  horoscope URL builder is ported as pure logic.")

(def zodiac-signs
  {1 "Aries" 2 "Taurus" 3 "Gemini" 4 "Cancer" 5 "Leo" 6 "Virgo"
   7 "Libra" 8 "Scorpio" 9 "Sagittarius" 10 "Capricorn" 11 "Aquarius"
   12 "Pisces"})

(defn horoscope-url
  "Horoscope page URL for sign number `sign` and `day`."
  [sign day]
  (str "https://www.horoscope.com/us/horoscopes/general/"
       "horoscope-general-daily-" day ".aspx?sign=" sign))

(defn horoscope
  "STUB: would return the horoscope text for `sign` and `day`."
  [sign day]
  (throw (ex-info "Network access required: GET horoscope.com"
                  {:reason :network-required :url (horoscope-url sign day)})))
