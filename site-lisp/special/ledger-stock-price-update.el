(require 'dash)

(defun my-update-ledger-stock-quotes (ticker)
  (let ((data (let ((limit (org-time-string-to-time "2015-10-01")))
                (-filter (-lambda ((d)) (not (time-less-p (org-time-string-to-time d) limit)))
                         (stocklist-get-data-quandl ticker)))))
    (with-temp-file (format "/home/matus/org/ledger-prices/%s.ledger" ticker)
      (--each (nreverse data)
        (insert (format
                 "P %s 00:00:00 %s $%.2f\n"
                 (replace-regexp-in-string "-" "/" (car it))
                 ticker
                 (nth 4 it)))))))

(defun my-update-ledger-prices ()
  (let ((stocks (list
                 "AAPL"
                 "DIS"
                 "GLF"
                 "GM"
                 "JWN"
                 "M"
                 ;; "NMM" ; broken
                 "T"
                 "TDW"
                 "VLO"
                 "WFC"
                 )))
    (--each stocks
      (my-update-ledger-stock-quotes it))))

(defun my-ofx-to-ledger ()
  "Transform ofx monthly data to ledger format.

https://www.ofx.com/en-au/forex-news/historical-exchange-rates/monthly-average-rates/"
  (interactive)
  (let ((months '(("Jan" . "01")
                  ("Feb" . "02")
                  ("Mar" . "03")
                  ("Apr" . "04")
                  ("May" . "05")
                  ("Jun" . "06")
                  ("Jul" . "07")
                  ("Aug" . "08")
                  ("Sep" . "09")
                  ("Oct" . "10")
                  ("Nov" . "11")
                  ("Dec" . "12"))))
    (-each months
      (-lambda ((month . number))
        (goto-char (point-min))
        (while (re-search-forward month nil t)
          (replace-match number))))
    (goto-char (point-min))
    (my-with-each-line
      (-let (((day month year price)
              (->> (thing-at-point 'line)
                   (s-trim)
                   (s-split "[[:blank:]]"))))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (format "P %s/%s/%s 00:00:00 $ %s Kc"
                        year month day
                        price))))))
