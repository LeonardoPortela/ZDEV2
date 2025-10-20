*&--------------------------------------------------------------------&*
*&                        Wayon                                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 15.10.2020                                              &*
*& Descrição: Enhancement USEREXIT_XKOMV_FUELLEN                      &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

FORM f_frete_perc_valor_min.

  FIELD-SYMBOLS: <f_sdabw> TYPE any,
                 <f_tknum> TYPE any.

  DATA: l_kwert_zfvm      TYPE komv-kwert,
        l_kwert_zvfp      TYPE komv-kwert,
        l_kwert_zfre      TYPE komv-kwert,
        l_kwert_zfre_zvfp TYPE komv-kwert,
        l_kawrt_zfvm      TYPE komv-kawrt,
        l_kawrt_zvfp      TYPE komv-kawrt,
        l_kawrt_zfre      TYPE komv-kawrt,
        l_kawrt_zfre_zvfp TYPE komv-kawrt,
        l_kbetr_zfre      TYPE komv-kbetr,
        l_valor(15)       TYPE p DECIMALS 3,
        l_valor_c(15)     TYPE c,
        l_strlen          TYPE i,
        l_sdabw           TYPE vttk-sdabw,
        l_perc            TYPE komv-kbetr,
        l_ativa           TYPE c.

*-#133089-21.02.2024-JT-inicio
  DATA: t_callstack  TYPE abap_callstack,
        lc_fat_autom TYPE char01.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = 0
    IMPORTING
      callstack = t_callstack.

*------------------
* faturamento automatico
*------------------
  READ TABLE t_callstack INTO DATA(w_callstack) WITH KEY mainprogram = 'ZLESR0180_JOB'.
  IF sy-subrc = 0.
    lc_fat_autom = abap_true.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  FREE: l_ativa.

*-----------------------------------------------
* Ativa/desativa EXIT
*-----------------------------------------------
  SELECT low
    INTO l_ativa
    FROM tvarvc
      UP TO 1 ROWS
   WHERE name = 'ZSD_FRETE_VALOR_MINIMO'.
  ENDSELECT.

  IF sy-subrc <> 0.
    l_ativa = abap_true.
  ENDIF.

  CHECK l_ativa = abap_true.

  CHECK sy-tcode EQ 'VI01'
     OR sy-tcode EQ 'VI02'
     OR sy-tcode EQ 'VI03'
     OR sy-tcode EQ 'VT01N'
     OR sy-tcode EQ 'VT01'
     OR sy-tcode EQ 'VT02N'
     OR sy-tcode EQ 'VT02'
     OR sy-tcode EQ 'VT03N'
     OR sy-tcode EQ 'VT03'
     OR sy-tcode EQ 'ZLES0136'
     OR sy-tcode EQ 'ZLES0115'
     OR lc_fat_autom = abap_true.  "*-#133089-21.02.2024-JT

  FREE: l_kwert_zfvm,
        l_kwert_zvfp,
        l_kwert_zfre,
        l_kwert_zfre_zvfp,
        l_kawrt_zfvm,
        l_kawrt_zvfp,
        l_kawrt_zfre,
        l_kawrt_zfre_zvfp.

*------------------------------------------------
* ajusta condicao ZVFP
*------------------------------------------------
  READ TABLE xkomv WITH KEY kposn = 0.
  IF sy-subrc <> 0.
    READ TABLE xkomv WITH KEY kschl = 'ZVFP'.
    IF sy-subrc = 0 AND xkomv-kwert <> 0.
      FREE l_kwert_zfre.
      LOOP AT xkomv WHERE kschl = 'ZFRE'.
        l_kwert_zfre = l_kwert_zfre + xkomv-kwert.
      ENDLOOP.
      READ TABLE xkomv WITH KEY kschl = 'ZVFP'.
      xkomv-kwert = l_kwert_zfre.
      MODIFY xkomv INDEX sy-tabix.
    ENDIF.
  ENDIF.

  FREE: l_kwert_zfre.

*------------------------------------------------
* elimina ZFVP se nao e frete percentual
*------------------------------------------------
  FREE: l_sdabw.

  ASSIGN ('(SAPMV56A)VTTK-SDABW')  TO <f_sdabw>.
  IF sy-subrc = 0.
    l_sdabw = <f_sdabw>.
    IF l_sdabw IS INITIAL.
      ASSIGN ('(SAPMV54A)VTTK-TKNUM')  TO <f_tknum>.
      IF sy-subrc = 0.
        SELECT SINGLE sdabw
                 INTO l_sdabw
                 FROM vttk
                WHERE tknum = <f_tknum>.
      ENDIF.
    ENDIF.

    IF l_sdabw <> '0003' AND l_sdabw IS NOT INITIAL.
      LOOP AT xkomv.
        IF xkomv-kschl = 'ZVFP'.
          DELETE xkomv INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

*------------------------------------------------
* ajusta ZFRE
*------------------------------------------------
  LOOP AT xkomv.
    CASE  xkomv-kschl.
      WHEN 'ZFRE'.
        l_kwert_zfre_zvfp = l_kwert_zfre_zvfp + xkomv-kwert.
        l_kawrt_zfre_zvfp = l_kawrt_zfre_zvfp + xkomv-kawrt.
        l_kbetr_zfre      = xkomv-kbetr.
      WHEN 'ZFVM'.
        l_kwert_zfvm = l_kwert_zfvm + xkomv-kwert.
        l_kawrt_zfvm = l_kawrt_zfvm + xkomv-kawrt.
      WHEN 'ZVFP'.
        l_kwert_zvfp = l_kwert_zvfp + xkomv-kwert.
        l_kawrt_zvfp = l_kawrt_zvfp + xkomv-kawrt.
    ENDCASE.
  ENDLOOP.

  CHECK l_kwert_zfvm IS NOT INITIAL AND
        l_kwert_zvfp IS NOT INITIAL.

  IF l_kwert_zfvm > l_kwert_zvfp.
    l_kwert_zfre =  l_kwert_zfvm.
    l_kawrt_zfre =  l_kawrt_zfvm.
  ELSE.
    l_kwert_zfre =  l_kwert_zvfp.
    l_kawrt_zfre =  l_kawrt_zvfp.
  ENDIF.

  LOOP AT xkomv  WHERE kschl = 'ZFRE'
                    OR kschl = 'ZVFP'.
    CASE xkomv-kschl.
      WHEN 'ZFRE'.
        xkomv-kwert      = l_kwert_zfre.
        komp-netwr       = l_kwert_zfre.
*       xkomv-kawrt      = l_kawrt_zfre.
      WHEN 'ZVFP'.
        l_perc           = l_kbetr_zfre / 1000.
        l_valor          = ( l_kawrt_zfre_zvfp * l_perc ) / 10.
        l_valor_c        = l_valor.
        CONDENSE l_valor_c NO-GAPS.
        l_strlen         = strlen( l_valor_c ) - 1.
        l_valor_c        = l_valor_c(l_strlen).
        xkomv-kwert      = l_valor_c.
*       xkomv-kwert      = l_kwert_zfre_zvfp.
*       xkomv-kawrt      = l_kawrt_zfre_zvfp.
    ENDCASE.
    MODIFY xkomv INDEX sy-tabix.
  ENDLOOP.
ENDFORM.

*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
