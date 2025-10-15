*&---------------------------------------------------------------------*
*& Include ZLESR0102_FORM2
*&---------------------------------------------------------------------*

FORM f_vincula_zles0200 USING p_rows TYPE lvc_t_row.

  DATA: w_saida  TYPE ty_saida.

  READ TABLE p_rows INTO DATA(w_rows) INDEX 1.

  w_saida = it_saida[ w_rows-index ].

*-CS2024000086-25.09.2024-#133287-JT-inicio
  IF w_saida-danfe(1) = '@'.
    MESSAGE s024(sd) WITH 'Deve ser autorizada a NF-e!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-CS2024000086-25.09.2024-#133287-JT-fim

  SELECT SINGLE *
    INTO @DATA(_j_1bnflin)
    FROM j_1bnflin
   WHERE docnum = @w_saida-danfe.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'NF não Encontrada!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

*---------------------------------------------
*- set MAGGI_CFOP_VENDA_IND
*---------------------------------------------
  SELECT SINGLE *
    FROM setleaf
    INTO @DATA(w_setleaf)
   WHERE setname = 'MAGGI_CFOP_VENDA_IND'
     AND valfrom = @_j_1bnflin-cfop.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'NF selecionada não é Venda por Conta e Ordem!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

*---------------------------------------------
*- tvarv MAGGI_GR_FERTILIZANTES
*---------------------------------------------
  SELECT SINGLE *
    FROM tvarvc
    INTO @DATA(_tvarvc)
   WHERE name = 'MAGGI_GR_FERTILIZANTES'
     AND low  = @_j_1bnflin-matkl.

  IF sy-subrc <> 0.
    MESSAGE s024(sd) WITH 'NF selecionada não é Venda por Conta e Ordem!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

*---------------------------------------------
*- recuperar parametros
*---------------------------------------------
  SELECT SINGLE docdat
    INTO @DATA(_docdat)
    FROM j_1bnfdoc
   WHERE docnum = @w_saida-danfe.

*---------------------------------------------
*- executa zles0200
*---------------------------------------------
  SUBMIT zlesr0152 WITH p_nfterc     = abap_true
                   WITH p_opcao1     = abap_true
                   WITH s_werks-low  = w_saida-branch
                   WITH s_vbeln-low  = w_saida-vbeln
                   WITH s_datanf-low = _docdat
                   WITH s_docnum     = _j_1bnflin-docnum
                   WITH p_popup      = abap_true
               AND RETURN.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
