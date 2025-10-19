*&---------------------------------------------------------------------*
*&  Include           ZSDI0021
*&---------------------------------------------------------------------*
* Ajuste para incluir PIS e COFINS na BC do ICMS
  CONSTANTS c_m3 TYPE meinh VALUE 'M3'.
  FIELD-SYMBOLS: <fs_xvbap> TYPE vbapvb,
                 <fs_xvbrp> TYPE vbrpvb.

  DATA: lv_quant_m3        TYPE dzmeng,
        lv_quant           TYPE dzmeng,
        lv_uom             TYPE vrkme,
        ls_vbap            TYPE vbap,
        ls_vbrp            TYPE vbrp,
        lv_vlr_cofins      TYPE kwert,
        lv_vlr_pis         TYPE kwert,
        lv_aumento_bc_icms TYPE kwert.

  READ TABLE xkomv TRANSPORTING NO FIELDS WITH KEY kschl = 'PR00'.      "Preço
  IF sy-subrc IS INITIAL.

    ASSIGN ('(SAPMV60A)XVBRP') TO <fs_xvbrp>.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING <fs_xvbrp> TO ls_vbrp.
      lv_quant = ls_vbrp-fkimg.
      lv_uom   = ls_vbrp-vrkme.
    ELSE.
      ASSIGN ('(SAPMV45A)XVBAP') TO <fs_xvbap>.
    IF sy-subrc IS INITIAL.   "<<SKM
      MOVE-CORRESPONDING <fs_xvbap> TO ls_vbap.
      lv_quant = ls_vbap-kwmeng.
      lv_uom   = ls_vbap-vrkme.
    endif.

    IF sy-subrc IS NOT INITIAL and lv_quant is INITIAL.
       ASSIGN ('(SAPLVBRK)XVBRP') TO <fs_xvbrp>.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING <fs_xvbrp> TO ls_vbrp.
      lv_quant = ls_vbrp-fkimg.
      lv_uom   = ls_vbrp-vrkme.
    endif.


    endif.





    ENDIF.

    IF lv_quant IS NOT INITIAL
   AND lv_uom   IS NOT INITIAL.
* Base de Cálculo Biodiesel - Quantidade em m³
      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input    = lv_quant
          unit_in  = lv_uom
          unit_out = c_m3
        IMPORTING
          output   = lv_quant_m3
        EXCEPTIONS
          OTHERS   = 10.

      IF sy-subrc IS INITIAL.

* Alíquota ICMS
        READ TABLE xkomv INTO DATA(ls_aliq_icms) WITH KEY kschl = 'ICVA'.
        IF sy-subrc IS NOT INITIAL.
          CLEAR ls_aliq_icms.
        ENDIF.

* Valor total de COFINS
        READ TABLE xkomv INTO DATA(ls_komv_aliq) WITH KEY kschl = 'ZBCO'.     "Alíquota
        READ TABLE xkomv INTO DATA(ls_komv_red_bc) WITH KEY kschl = 'ZBBC'.   "Redução de BC

        lv_vlr_cofins = ls_komv_aliq-kbetr * ls_komv_red_bc-kbetr * lv_quant_m3 / 100000000.

* Valor total de PIS
        READ TABLE xkomv INTO ls_komv_aliq WITH KEY kschl = 'ZBPI'.           "Alíquota
        READ TABLE xkomv INTO ls_komv_red_bc WITH KEY kschl = 'ZBBP'.         "Redução de BC

        lv_vlr_pis = ls_komv_aliq-kbetr * ls_komv_red_bc-kbetr * lv_quant_m3 / 100000000.

        TRY.
* Fazendo a conta para que o valor entre por dentro, como é feito com o ICMS.
            lv_aumento_bc_icms = ( lv_vlr_cofins + lv_vlr_pis ). " * 100 / ( 100 - ls_aliq_icms-kbetr / 1000 ).
          CATCH cx_sy_zerodivide.
            CLEAR lv_aumento_bc_icms.
        ENDTRY.

        IF lv_vlr_pis         IS NOT INITIAL
       AND lv_vlr_cofins      IS NOT INITIAL
       AND ls_aliq_icms-kbetr IS NOT INITIAL.
          xkomv-kwert = xkwert = xkwert + lv_aumento_bc_icms.
        ENDIF.

      ENDIF.
    ENDIF.
  ENDIF.
