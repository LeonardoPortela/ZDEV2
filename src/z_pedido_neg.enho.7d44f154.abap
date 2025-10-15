"Name: \PR:SAPLMBGB\FO:FELDER_WE_INIT\SE:BEGIN\EI
ENHANCEMENT 0 Z_PEDIDO_NEG.
*
*
*  data V_BSART type ekko-bsart.
*
**  if sy-batch = 'X'.
*   select SINGLE BSART
*     from ekko
*     into V_BSART
*     where ebeln = XEKWS-EBELN.
*   if V_BSART = 'ZGR'.


" O preço do movimento de mercadorias torna-se negativo (evitar essa mensagem)
  data T_PEDIDO                TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      CLASS         = '0000'
      SETNR         = 'MAGGI_PEDIDO_NEG'
    TABLES
      SET_VALUES    = T_PEDIDO
    EXCEPTIONS
      SET_NOT_FOUND = 1
      OTHERS        = 2.
  IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  SORT T_PEDIDO BY FROM.

  READ TABLE T_PEDIDO with key from = XEKWS-EBELN.
  IF sy-subrc = 0.
     CLEAR: g_maa_use_order_price_unit.                        "MAA2

*     form is almost replaced by note 398254
      IF dm07m-mulko = gc_mulko_3.                              "MAA2
        mengb       = xekws-wwmng_abd - dm07m-bstmg_abd.
        remng_abd   = xekws-remng_abd.
        we_re_menge = xekws-wwmng_abd - xekws-remng_abd.
        IF we_re_menge IN cl_mmim_maa_2=>gt_range_menge_abd.
          CLEAR we_re_menge.  "set small qty to zero
        ENDIF.
        bsmng       = dm07m-bsmng_abd.
        erfmg       = dm07m-bstmg_abd.
      ELSE.
        mengb = xekws-wwmng - mseg-bstmg.
        erfmg = mseg-bstmg.
        bsmng = dm07m-bsmng.
        remng = xekws-remng.
        we_re_menge = xekws-wwmng - xekws-remng.
      ENDIF.

      IF NOT dm07m-mulko IS INITIAL.                            "MAA2
        we_erfmg_maa = xekws-wwmng + erfmg.
        IF dm07m-mulko = gc_mulko_3.
          we_erfmg_maa = xekws-wwmng_abd + erfmg.
        ENDIF.
      ENDIF.                                                    "MAA2
      we_re_wert = xekws-wewrt - xekws-arewr.

      DATA: l_bpmng_with_sign2 LIKE mseg-bpmng,
            l_bpmng_with_sign_abd2 LIKE dm07m-bpmng_abd.         "MAA2

      IF ( mseg-shkzg = s AND vm07m-retpo IS INITIAL ) OR
        ( mseg-shkzg = h AND NOT vm07m-retpo IS INITIAL ).
        we_re_menge_final = we_re_menge + erfmg.
        IF dm07m-mulko = gc_mulko_3.                            "MAA2
          l_bpmng_with_sign_abd2 = dm07m-bpmng_abd.
        ELSE.                                                   "MAA2
          l_bpmng_with_sign2 = mseg-bpmng.
        ENDIF.                                                  "MAA2
      ELSE.
        we_re_menge_final = we_re_menge - erfmg.
        IF dm07m-mulko = gc_mulko_3.                            "MAA2
          l_bpmng_with_sign_abd2 = 0 - dm07m-bpmng_abd.
        ELSE.                                                   "MAA2
          l_bpmng_with_sign2 = 0 - mseg-bpmng.
        ENDIF.                                                  "MAA2
      ENDIF.

*     The indicator that invoiced quantity is > goods
*     receipt quantity in order unit after posting is WE_RE_MENGE_FINAL
*     > 0.

      IF NOT mseg-bprme IS INITIAL AND we_re_menge_final > 0.
        g_maa_use_order_price_unit = 'X'.
*     MAA2 Begin
        IF dm07m-mulko = gc_mulko_3.
          mengb = xekws-bpwwm_abd - dm07m-bpmng_abd.
          erfmg = dm07m-bpmng_abd.
          bsmng = dm07m-bpbmg_abd.
          remng = xekws-bprem_abd.
          we_re_menge = xekws-bpwwm_abd - xekws-bprem_abd.
          IF we_re_menge IN cl_mmim_maa_2=>gt_range_menge_abd.
            CLEAR we_re_menge.  "set small qty to zero
          ENDIF.
          we_re_menge_final = we_re_menge + l_bpmng_with_sign_abd2.
          we_erfmg_maa = xekws-bpwwm_abd + erfmg.
        ELSE.
*     MAA2 End
          mengb = xekws-bpwwm - mseg-bpmng.
          erfmg = mseg-bpmng.
          bsmng = dm07m-bpbmg.
          remng = xekws-bprem.
          we_re_menge = xekws-bpwwm - xekws-bprem.
          we_re_menge_final = we_re_menge + l_bpmng_with_sign2.
*     MAA2 Begin
          IF dm07m-mulko = gc_mulko_1 OR dm07m-mulko = gc_mulko_2.
            we_erfmg_maa = xekws-bpwwm + erfmg.
          ENDIF.
        ENDIF.
*     MAA2 End
      ENDIF.
      EXIT.
   endif.

ENDENHANCEMENT.
