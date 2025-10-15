*----------------------------------------------------------------------*
***INCLUDE ZFIR0125_PBO_0400.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0400 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0400 OUTPUT.

  SET PF-STATUS 'STATUS_0400'.
***      Ocutar campos de recebimento parcial.
  PERFORM ocutar_campos_juros.
  PERFORM ocutar_campos_fatura.

  IF ind_rec_parc IS NOT INITIAL.
    PERFORM ocutar_campo_rec_parc.
  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_JUROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_juros .


  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXT_JUROS'                   OR 'TXTTAXA_JUROS'
        OR 'JUROS'                       OR 'TXT_MULT'
        OR 'TXT_TOT_OV'                  OR '100'
        OR 'TXT_PORC_JUROS'              OR 'TXTVLR_JUROS_PARC'
        OR 'TXT_JROS'                    OR 'TXT_SALD_OV'
        OR '%#AUTOTEXT003'               OR '%#AUTOTEXT007'
        OR 'TXTCAL_VALR_TAXA_JUROS_PARC' OR '%#AUTOTEXT006'
        OR 'TXT_SUB'                     OR 'TXT_MAIS'
        OR 'TXT_MAISS'                   OR 'TXT_SUB'
        OR 'TXT_SUBB'                    OR '%#AUTOTEXT001'
        OR '%#AUTOTEXT002'               OR 'TXT_P'
        OR 'TXT_PO'                      OR 'WA_EDIT-JROS'
        OR 'W_EDIT-JUROS'                OR '%#AUTOTEXT008'
        OR 'WA_EDIT-PORC_JUROS'          OR 'W_EDIT-PORC_JUROS'
        OR 'WA_EDIT-MULTA'               OR 'WA_EDIT-VLR_RBDO'
        OR 'W_EDIT-VLR_TOTAL_OV'         OR 'W_EDIT-POR'
        OR 'W_EDIT-PORC'                 OR 'WA_EDIT-JUROS_PARC'.

        IF NOT ind_rec_total IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.


ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPOS_FATURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campos_fatura .
  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'W_EDIT-DOC_FATURA' OR 'TXT_FATURA'.
        IF ind_doc_fatura IS NOT INITIAL.
          screen-invisible = 0. "Campo Fechado
          MODIFY SCREEN.
        ELSE.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  OCUTAR_CAMPO_REC_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ocutar_campo_rec_parc .

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'TXTVLR_JUROS' OR
           'TXT_FAT' OR
           'TXT_T_OV' OR
           'TXT_DV' OR
           '%#AUTOTEXT009' OR
           'TXT_100' OR
           '%#AUTOTEXT005' OR
           'TXTCAL_VALR_' OR
           'W_EDIT-FATOR' OR
           'WA_EDIT-VLR_TOTAL_OV' OR
           'WA_EDIT-PORC' OR
           'WA_EDIT-JUROS' OR
           '%#AUTOTEXT001' OR
           'TXTTAXA_JUROS' OR
           '%#AUTOTEXT002' OR
           'TXTINDICADOR' OR
           'TXT_JUROS' OR
           '%#AUTOTEXT007' OR
           'JUROS' OR
           'TXT_MAIS' OR
           'TXT_MULT' OR
           'TXT_MAISS' OR
           'TXT_TOT_OV' OR
           'TXT_SUB' OR
           'TXT_PO' OR
           '%#AUTOTEXT006' OR
           'TXT_PORC_JUROS' OR
           'WA_EDIT-JROS' OR
           'W_EDIT-JUROS' OR
           'WA_EDIT-MULTA' OR
           'W_EDIT-VLR_TOTAL_OV' OR
           'W_EDIT-PORC' OR
           'WA_EDIT-PORC_JUROS'.

        IF NOT ind_rec_parc IS INITIAL.
          screen-invisible = 1. "Campo Fechado
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFORM.
