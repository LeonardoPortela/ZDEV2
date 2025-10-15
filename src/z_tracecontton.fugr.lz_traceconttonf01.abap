*----------------------------------------------------------------------*
***INCLUDE LZ_TRACECONTTONF01.
*----------------------------------------------------------------------*
**********************************************************************
* carregar planilha
**********************************************************************
FORM f_carrega_file CHANGING p_erro.

  DATA: l_erro TYPE char1,
        l_file TYPE rlgrap-filename,
        l_rows TYPE i,
        l_cols TYPE i.

  FREE: t_file, p_erro.

  l_file = g_file_name.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Carregando Planilha...'.

*----------------------------------------
* upload excel
*----------------------------------------
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = l_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 100
      i_end_row               = 30000
*     i_end_col               = 256
*     i_end_row               = 65536
    TABLES
      intern                  = t_tab
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Falha ao abrir Planilha Excel.' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*----------------------------------------
* carrega tabela interna
*----------------------------------------
  FREE: l_erro, l_cols, l_rows.

  LOOP AT t_tab INTO w_tab.
    l_cols = l_cols + 1.
    ASSIGN COMPONENT w_tab-col OF STRUCTURE w_file TO <fs_fld>.
    <fs_fld> = w_tab-value.
    AT END OF row.
      l_rows          = l_rows + 1.
      w_file-linha    = l_rows.
      w_file-erro     = abap_off.
      w_file-mensagem = abap_off.
      APPEND w_file  TO t_file.
      FREE: l_cols, w_file.
    ENDAT.
  ENDLOOP.

  READ TABLE t_file INTO w_file INDEX 1.
  IF sy-subrc = 0.
    DELETE t_file INDEX 1.
  ENDIF.

ENDFORM.

**********************************************************************
* valida arquivo
**********************************************************************
FORM f_valida_arquivo CHANGING p_erro.

  FREE: p_erro.

  LOOP AT t_file INTO w_file.

    l_tabix = sy-tabix.

*--------------------------------------
*-- formatacao campos
*--------------------------------------
    CONDENSE w_file-lote             NO-GAPS.
    CONDENSE w_file-tipo             NO-GAPS.
    CONDENSE w_file-cod_material     NO-GAPS.
    CONDENSE w_file-qtd_fardos       NO-GAPS.
    CONDENSE w_file-peso_lote        NO-GAPS.
    CONDENSE w_file-motivo           NO-GAPS.
    CONDENSE w_file-cod_filial       NO-GAPS.
    CONDENSE w_file-contrato         NO-GAPS.
    CONDENSE w_file-tam_fardo        NO-GAPS.
    CONDENSE w_file-data_takeup      NO-GAPS.
    CONDENSE w_file-empresa          NO-GAPS.
    CONDENSE w_file-fornecedor       NO-GAPS.

    l_data_char         = w_file-data_takeup.
    w_file-data_takeup  = l_data_char+6(4) && l_data_char+3(2) && l_data_char(2).
    w_file-empresa      = |{ w_file-empresa      ALPHA = IN }|.
    w_file-cod_filial   = |{ w_file-cod_filial   ALPHA = IN }|.

*** Stefanini - IR256815 - 08/09/2025 - LAZAROSR - Início de Alteração
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = w_file-cod_material
        IMPORTING
          output = w_file-cod_material.

*    w_file-cod_material = |{ w_file-cod_material ALPHA = IN }|.
*** Stefanini - IR256815 - 08/09/2025 - LAZAROSR - Fim de Alteração

    w_file-fornecedor   = |{ w_file-fornecedor   ALPHA = IN }|.

    PERFORM f_change_text CHANGING w_file-peso_lote.

*--------------------------------------
*-- valida empresa
*--------------------------------------
    IF w_file-empresa <> l_bukrs.
      w_file-erro     = abap_true.
      w_file-mensagem = 'Código da empresa divergente do informado na tela de filtro.'.
      MODIFY t_file FROM w_file INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------------
*-- valida material
*--------------------------------------
    SELECT matnr
      INTO @DATA(_matnr)
      FROM mara
        UP TO 1 ROWS
     WHERE matnr = @w_file-cod_material
       AND matkl = '700140'
*       AND mtart = 'ZHAW'.                         "<<RIM-SKM-IR117990-14.11.22
       AND  ( mtart = 'ZHAW' OR mtart = 'ZFER' ).   ">>RIM-SKM-IR117990-14.11.22
    ENDSELECT.
    IF sy-subrc <> 0.
      w_file-erro     = abap_true.
      w_file-mensagem = 'Cód Material informado não é de produto Algodão Revenda'.
      MODIFY t_file FROM w_file INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------------
*-- valida filial
*--------------------------------------
    SELECT werks
      INTO @DATA(_werks)
      FROM t001w
        UP TO 1 ROWS
     WHERE werks = @w_file-cod_filial
       AND vkorg = @w_file-empresa.
    ENDSELECT.
    IF sy-subrc <> 0.
      w_file-erro     = abap_true.
      w_file-mensagem = 'Filial:' && w_file-cod_filial && ' não cadastrada para a Empresa:' && w_file-empresa.
      MODIFY t_file FROM w_file INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------------
*-- valida contrato
*--------------------------------------
    SELECT contrato
      INTO @DATA(_contrato)
      FROM zsdt0143
        UP TO 1 ROWS
     WHERE contrato  = @w_file-contrato
       AND empresa   = @w_file-empresa
       AND cancelado = @abap_off.
    ENDSELECT.
    IF sy-subrc <> 0.
      w_file-erro     = abap_true.
      w_file-mensagem = 'Contrato:' && w_file-contrato && ' não localizado no SAP para a Empresa:' && w_file-empresa.
      MODIFY t_file FROM w_file INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------------
*-- valida fornecedor
*--------------------------------------
    SELECT lifnr
      INTO @DATA(_lifnr)
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr   = @w_file-fornecedor.
    ENDSELECT.
    IF sy-subrc <> 0.
      w_file-erro     = abap_true.
      w_file-mensagem = 'Fornecedor:' && w_file-fornecedor && ' não cadastrado nos dados mestre do SAP'.
      MODIFY t_file FROM w_file INDEX l_tabix.
      CONTINUE.
    ENDIF.

*--------------------------------------
*-- valida lote
*--------------------------------------
    IF strlen( w_file-lote ) > 4.
      w_file-erro     = abap_true.
      w_file-mensagem = 'Lote:' && w_file-lote && ' não pode ter mais que 4 dígitos'.
      MODIFY t_file FROM w_file INDEX l_tabix.
      CONTINUE.
    ENDIF.

*-CS2023000189-05.04.2023-#108694-JT-inicio
*-verifica se ha mais de umcontrato com ACTS diferentes entre si
    CLEAR l_erro.

    CLEAR: l_acts_true, l_acts_false.

    SELECT empresa, contrato, safra, acts
      FROM zsdt0143
      INTO TABLE @DATA(t_143)
     WHERE contrato     = @w_file-contrato
       AND empresa      = @w_file-empresa
       AND cancelado   <> @abap_true.

    IF sy-subrc = 0.
      LOOP AT t_143 INTO DATA(w_143).
        IF w_143-acts = abap_true.
          l_acts_true = abap_true.
        ELSE.
          l_acts_false = abap_true.
        ENDIF.
      ENDLOOP.

      IF l_acts_true = abap_true AND l_acts_false = abap_true.
        l_erro = abap_true.
      ENDIF.
    ENDIF.

    IF l_erro = abap_true.
      w_file-erro     = abap_true.
      w_file-mensagem = 'Há ACTS incompatíveis entre contratos!'.
      MODIFY t_file FROM w_file INDEX l_tabix.
      CONTINUE.
    ENDIF.
*-CS2023000189-05.04.2023-#108694-JT-fim

    MODIFY t_file FROM w_file INDEX l_tabix.
  ENDLOOP.

  READ TABLE t_file INTO w_file WITH KEY erro = abap_true.
  IF sy-subrc = 0.
    p_erro = abap_true.
  ENDIF.

ENDFORM.

**********************************************************************
* exibe erros do arquivo
**********************************************************************
FORM f_exibe_erros.

  DELETE t_file WHERE erro = abap_off.

  CALL FUNCTION 'ZSD_EXIBIR_ERROS_TAKEUP'
    TABLES
      t_dados = t_file.

ENDFORM.

**********************************************************************
* gravar dados
**********************************************************************
FORM f_grava_dados.

  DATA: l_resp  TYPE c,
        l_texto TYPE string,
        l_lote  TYPE numc4,
        l_linc  TYPE char10.

  DESCRIBE TABLE t_file LINES DATA(l_linhas).
  l_linc = l_linhas.

  IF l_linhas > 0.
    CONCATENATE 'Serão Importados' l_linc 'Registros de Take-Up!'
           INTO l_texto SEPARATED BY space.
  ELSE.
    l_texto = 'Não há registros a serem Importados!'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação de Carga Take-Up'
      text_question         = l_texto
      text_button_1         = 'Confirma'
      text_button_2         = 'Cancela'
      icon_button_1         = 'ICON_SYSTEM_OKAY'
      icon_button_2         = 'ICON_SYSTEM_CANCEL'
      display_cancel_button = ' '
    IMPORTING
      answer                = l_resp
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK l_resp = '1' AND l_linhas > 0.

*---------------------------------------
* Gravar dados
*---------------------------------------
  LOOP AT t_file INTO w_file.

    CLEAR w_zsdt0166.

*---------------------
*-- proximo numero
*---------------------
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZSEQ0165'
      IMPORTING
        number                  = l_seq
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    l_lote = w_file-lote.

    SELECT cliente, preco, safra, acts
      INTO @DATA(_0143)
      FROM zsdt0143
        UP TO 1 ROWS
     WHERE contrato  = @w_file-contrato
       AND empresa   = @w_file-empresa
       AND cancelado = @abap_off.
    ENDSELECT.

    SELECT normt
      INTO @DATA(_normt)
      FROM mara
        UP TO 1 ROWS
     WHERE matnr = @w_file-cod_material
       AND matkl = '700140'
*       AND mtart = 'ZHAW'.                         "<<RIM-SKM-IR117990-14.11.22
       AND  ( mtart = 'ZHAW' OR mtart = 'ZFER' ).   ">>RIM-SKM-IR117990-14.11.22
    ENDSELECT.

    SELECT name1
      INTO @DATA(_name1)
      FROM lfa1
        UP TO 1 ROWS
     WHERE lifnr = @w_file-fornecedor.
    ENDSELECT.

    w_zsdt0166-mandt         = sy-mandt.
    w_zsdt0166-id            = l_seq.
    w_zsdt0166-data          = sy-datum.
    w_zsdt0166-hora          = sy-uzeit.
    w_zsdt0166-status        = 'A'.
    w_zsdt0166-lote          = l_lote.
    w_zsdt0166-kunnr         = _0143-cliente.
    w_zsdt0166-matnr         = w_file-cod_material.
    w_zsdt0166-tipo          = _normt.
    w_zsdt0166-qtd_fardos    = w_file-qtd_fardos.
    w_zsdt0166-peso_lote     = w_file-peso_lote.
    w_zsdt0166-motivo        = w_file-motivo.
    w_zsdt0166-werks         = w_file-cod_filial.
    w_zsdt0166-algodoeira    = _name1.
    w_zsdt0166-contrato      = w_file-contrato.
    w_zsdt0166-tamanho_fardo = w_file-tam_fardo.
    w_zsdt0166-preco_ctr     = _0143-preco.
    w_zsdt0166-data_takeup   = w_file-data_takeup.
    w_zsdt0166-safra         = _0143-safra.
    w_zsdt0166-empresa       = w_file-empresa.
*   w_zsdt0166-acts          = _0143-acts.     "*-CS2023000189-05.04.2023-#108694-JT-inicio
    w_zsdt0166-fornecedor    = w_file-fornecedor.
    w_zsdt0166-carga_dados   = abap_true.
    w_zsdt0166-usnam         = sy-uname.
    w_zsdt0166-data_atual    = sy-datum.
    w_zsdt0166-hora_atual    = sy-uzeit.
    MODIFY zsdt0166       FROM w_zsdt0166.

    CLEAR: _0143, _normt, _name1.
  ENDLOOP.

  COMMIT WORK AND WAIT.

  g_importou = abap_true.

  MESSAGE s024(sd) WITH 'Informações foram gravadas.'.

ENDFORM.

**********************************************************************
* tratamento valores
**********************************************************************
FORM f_change_text CHANGING p_out_text.

  DATA: l_index     TYPE i,
        l_length    TYPE i,
        l_character TYPE c,
        l_allowed   TYPE string.

  TRANSLATE p_out_text TO LOWER CASE.

  l_allowed = '1234567890,'.
  l_length  = strlen( p_out_text ).
  l_index   = 0.

  WHILE l_length GT l_index.
    l_character = p_out_text+l_index(1).
    SEARCH l_allowed FOR l_character.
    IF sy-subrc NE 0.
      REPLACE l_character WITH space INTO p_out_text.
    ENDIF.
    l_index = l_index + 1.
  ENDWHILE.

  REPLACE ALL OCCURRENCES OF REGEX ',' IN p_out_text WITH '.'.
  CONDENSE p_out_text NO-GAPS.

ENDFORM.


**********************************************************************
**********************************************************************
