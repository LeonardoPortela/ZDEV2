 READ TABLE tg_rbkp into wg_rbkp
   with key belnr = wg_ekbe-belnr
            gjahr = wg_ekbe-gjahr.

 if sy-subrc is INITIAL.
   READ TABLE tg_lfa1 into wg_lfa1
     with key lifnr = wg_rbkp-lifnr.

 ENDIF.























