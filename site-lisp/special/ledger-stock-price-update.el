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
