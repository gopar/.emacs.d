(defun rei-calculate-monthly-mortage-payments (principal interest-rate loan-years )
  "M = P * [ r(1+r)^n
            ----------
            ((1+r)^n)-1 ]


Calculate the base monthly payment WITHOUT PMI and Proprty Taxes"
  (interactive)
  (let* ((r (/ (/ interest-rate 100.0) 12.0)) ;; monthly interest rate
         (n (* loan-years 12)) ;; number of payments in the total life loan
         (upper (* r (expt (+ r 1) n)))
         (lower (- (expt (+ r 1) n) 1 ))
         (everything (/ upper lower)))
    (* principal everything)))

(defun rei-calculate-monthly-PMI (principal PMI)
  "PMI = (principal * (PMI/100)) / 12"
  (/ (* principal (/ PMI 100.0)) 12.0))

(defun rei-calculate-monthly-property-tax (house-value property-tax)
  "TAX = (house-value * ( property-tax / 100)) / 12"
  (/ (* house-value (/ property-tax 100.0)) 12.0))

(defun rei-calculate-monthly-homeowners-insurnace (house-value rate)
  "INSURANCE = (house-value * (rate / 100)) / 12"
  (/ (* house-value ( / rate 100)) 12))

(defun monthly-mortage (house-value interest-rate loan-years pmi property-tax insurance)
  "Return monthly mortgage with PMI and property taxes included"
  (+ (rei-calculate-monthly-mortage-payments principal interest-rate loan-years)
     (rei-calculate-monthly-PMI principal pmi)
     (rei-calculate-monthly-property-tax house-value property-tax)
     (rei-calculate-monthly-homeowners-insurnace house-value insurance)))

(defun rei-monthly-mortgage (house-value interest-rate loan-years pmi property-tax insurance &optional down-payment-percent)
  (setq house-value (rei-normalize-price house-value))
  (setq principal (- house-value (rei-calculate-down-payment house-value down-payment-percent)))

  (setq pmi (if (< interest-rate 20)
                (rei-calculate-monthly-PMI principal pmi)
              0))

  (+ (rei-calculate-monthly-mortage-payments principal interest-rate loan-years)
     (rei-calculate-monthly-property-tax house-value property-tax)
     (rei-calculate-monthly-homeowners-insurnace house-value insurance)
     pmi))

(defun rei-calculate-down-payment (price percent-down &optional normalize)
  "Return down payment from PRICE and PERCENT-DOWN."
  (when normalize
    (setq price (rei-normalize-price price)))

  (setq percent-down (float percent-down))
  (setq percent-down (/ percent-down 100.0))

  (* price percent-down))

(defun rei-normalize-price (price)
  (* price 1000.0))

(defun rei-monthly-cashflow-after-expenses (cashflow mortgage &optional insurance vacancy repairs capex)
  (setq insurance (or insurance 60) ;; $720 a year (60 x 12)
        vacancy (or vacancy (* mortgage .05)) ;; 5% of mortgage
        repairs (or repairs (* mortgage .05)) ;; 5% of mortgage
        capex (or capex 180)) ;; $180 as default

  (- cashflow insurance vacancy repairs capex))

(defun rei-coc-roi (cashflow mortgage downpayment)
  (setq cashflow (rei-monthly-cashflow-after-expenses cashflow mortgage))

  (let ((yearly-cashflow (* 12 cashflow))
        (closing-costs 5000) ;; Random number for now
        )

    (* 100 (/ yearly-cashflow (+ downpayment closing-costs)))))

(provide 'rei)
