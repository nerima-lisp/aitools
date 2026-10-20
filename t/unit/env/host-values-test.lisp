(in-package #:aitools.env.test)

(describe "aitools.env.domain host values"
  (it-each (("GITHUB_TOKEN" t) ("AWS_SECRET_ACCESS_KEY" t) ("DB_PASSWORD" t) ("api_key" t)
            ("OPENAI_API_KEY" t) ("HOME" nil) ("TOKENIZER_PATH" nil) ("PATH" nil))
      "classifies ~A as secret: ~A"
      (name secret)
    (expect (secret-environment-name-p name) :to-be secret))

  (it "splits PATH in order without empty or repeated entries"
    (expect (split-search-path "/a::/b/:/a") :to-equal '("/a" "/b/")))

  (it "joins directories without doubling the slash"
    (expect (join-directory "/b/" "git") :to-equal "/b/git")
    (expect (join-directory "/a" "git") :to-equal "/a/git"))

  (it "takes the first nonblank output line"
    (expect (first-output-line (format nil "~%  git version 2.55.0  ~%more~%")) :to-equal "git version 2.55.0")
    (expect (first-output-line "") :to-be nil))

  (it "accepts IANA names and refuses anything that could leave the zoneinfo directory"
    (expect (valid-zone-name-p "America/New_York") :to-be-truthy)
    (expect (valid-zone-name-p "Etc/GMT+5") :to-be-truthy)
    (expect (valid-zone-name-p "../etc/passwd") :to-be-falsy)
    (expect (valid-zone-name-p "/usr/share/zoneinfo/UTC") :to-be-falsy)
    (expect (valid-zone-name-p "America//X") :to-be-falsy))

  (it "finds the local zone name from TZ and from the /etc/localtime link"
    (expect (zone-name-from-tz-variable ":Asia/Tokyo") :to-equal "Asia/Tokyo")
    (expect (zone-name-from-tz-variable "EST5EDT") :to-be nil)
    (expect (zone-name-from-localtime-link "/var/db/timezone/zoneinfo/Asia/Tokyo") :to-equal "Asia/Tokyo")
    (expect (zone-name-from-localtime-link "/etc/zoneinfo/../x") :to-be nil)))
