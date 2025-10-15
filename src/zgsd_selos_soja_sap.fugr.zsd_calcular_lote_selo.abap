FUNCTION zsd_calcular_lote_selo .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"     REFERENCE(I_SAFRA) TYPE  ZDEPM_SAFRA OPTIONAL
*"     REFERENCE(I_MATNR) TYPE  MATNR OPTIONAL
*"     REFERENCE(I_DATA_REF) TYPE  DATUM OPTIONAL
*"     REFERENCE(I_DOCNUM) TYPE  J_1BDOCNUM OPTIONAL
*"     REFERENCE(I_ZPMT0054) TYPE  ZPMT0054 OPTIONAL
*"  EXPORTING
*"     VALUE(E_LOTE) TYPE  ZDEPM_LOTE
*"     VALUE(E_DATA_FABRICACAO) TYPE  DATUM
*"     VALUE(E_DATA_VALIDADE) TYPE  DATUM
*"     VALUE(E_SELO) TYPE  ZIMAGEM
*"  EXCEPTIONS
*"      DOCUMENTO_NAO_AUTORIZADO
*"      DOCUMENTO_NAO_IMPRIMIR
*"----------------------------------------------------------------------

  FREE: e_lote,
        e_data_fabricacao,
        e_data_validade,
        e_selo,
        t_semana.

  l_data_ref = i_data_ref.

*-----------------------------------
* parametrizacao
*-----------------------------------
  IF i_zpmt0054 IS INITIAL.
    SELECT *
      FROM zpmt0054
      INTO w_zpmt0054
        UP TO 1 ROWS
     WHERE werks = i_werks
       AND safra = i_safra
       AND matnr = i_matnr.
    ENDSELECT.

    IF sy-subrc <> 0.
      RAISE documento_nao_autorizado.
      EXIT.
    ENDIF.
  ELSE.
    w_zpmt0054          = i_zpmt0054.
    w_zpmt0054-bloquear = abap_false.
    w_zpmt0054-imprimir = abap_true.
  ENDIF.

*-----------------------------------
* imprime selo?
*-----------------------------------
  IF w_zpmt0054-imprimir = abap_false.
    RAISE documento_nao_imprimir.
    EXIT.
  ENDIF.

*-----------------------------------
* NF reimpressao
*-----------------------------------
  IF i_docnum IS NOT INITIAL.
    SELECT *
      FROM zpmt0056
      INTO w_zpmt0056
        UP TO 1 ROWS
     WHERE docnum = i_docnum.
    ENDSELECT.

    IF sy-subrc = 0.
      e_lote            = w_zpmt0056-lote.
      e_selo            = w_zpmt0056-selo.
      e_data_fabricacao = w_zpmt0056-data_fabricacao.
      e_data_validade   = w_zpmt0056-data_validade.
      EXIT.
    ENDIF.
  ENDIF.

*-----------------------------------
* lote esta bloqueado para numeracao
*-----------------------------------
  IF w_zpmt0054-bloquear = abap_true.
    l_numero_lote = w_zpmt0054-lote.
  ELSE.
*-----------------------------------
* calcular lote
*-----------------------------------
    PERFORM f_calcular_lote  CHANGING l_numero_lote.
  ENDIF.

*-----------------------------------
* calcular datas
*-----------------------------------
  PERFORM f_calcular_datas   CHANGING l_data_fabricacao
                                      l_data_validade.

*-----------------------------------
* saida
*-----------------------------------
  e_lote            = l_numero_lote.
  e_data_fabricacao = l_data_fabricacao.
  e_data_validade   = l_data_validade.
  e_selo            = w_zpmt0054-imagem.

ENDFUNCTION.
