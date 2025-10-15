*&---------------------------------------------------------------------*
*& Report  ZSDR0014                                                    *
* Descrição  : Programa para ajuste dos adiantamentos atribuindo ordem *
* de venda aos adiantamentos, precisamos para atualizar o relatorio    *
*ZFIS19.                                                               *
* Módulo     : SD                               Transação: -           *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Camila Brand                            Data: 10/11/2011*
* Observações: Desenvolvimento inicial do Programa - DEVK919704+++     *
* Conforme chamado - 55393                                             *
* Alteração DEVK920053 - Conforme chamado 58212
*----------------------------------------------------------------------*

REPORT  zsdr0014.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES:
  bsad,           " Cabeçalho do documento de compra
  bsid,           " Contabilidade: índice secundário para clientes
  bseg.           " Segmento do documento contabilidade financeira

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA:
  it_bsad     TYPE TABLE OF bsad,
  it_bsid     TYPE TABLE OF bsid,
  it_bseg     TYPE TABLE OF bseg,
  it_zcot0007 TYPE TABLE OF zcot0007.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA:
  wk_bsad TYPE bsad,
  wk_bsid TYPE bsid,
  wk_bseg TYPE bseg.

*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

" Seleção de Campos (TextField)
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: x_bsad RADIOBUTTON GROUP rb1 USER-COMMAND sel DEFAULT 'X'.
PARAMETERS: x_bsid RADIOBUTTON GROUP rb1.
PARAMETERS: x_bseg RADIOBUTTON GROUP rb1.
PARAMETERS: x_soc  RADIOBUTTON GROUP rb1.
PARAMETERS: x_zib  RADIOBUTTON GROUP rb1.
PARAMETERS: x_regu  RADIOBUTTON GROUP rb1.

PARAMETERS: p_laufd TYPE reguh-laufd.
PARAMETERS: p_laufi TYPE reguh-laufi.

PARAMETERS: x_imob  RADIOBUTTON GROUP rb1.

PARAMETERS: p_afabe  TYPE anlc-afabe.
PARAMETERS: p_gjahri TYPE anlc-gjahr.

PARAMETERS: x_imob_e  RADIOBUTTON GROUP rb1.

PARAMETERS: p_bukrse   TYPE anlc-bukrs.
PARAMETERS: p_gjahre  TYPE anlc-gjahr.


SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: SKIP 1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: p_bukrs  FOR bsad-bukrs OBLIGATORY NO-EXTENSION NO INTERVALS,
                p_gjahr  FOR bsad-gjahr OBLIGATORY NO-EXTENSION NO INTERVALS,
                p_belnr  FOR bsad-belnr OBLIGATORY NO-EXTENSION NO INTERVALS,
                p_vbel2  FOR bsad-vbel2 OBLIGATORY NO-EXTENSION NO INTERVALS,
                p_cond   FOR bsad-zterm OBLIGATORY NO-EXTENSION NO INTERVALS,
                p_buzei  FOR bsad-zterm OBLIGATORY NO-EXTENSION NO INTERVALS,
                p_vbund  FOR bseg-vbund NO-EXTENSION NO INTERVALS.

PARAMETERS: p_import AS CHECKBOX.
PARAMETERS: p_file TYPE rlgrap-filename.

SELECTION-SCREEN: END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_filename     = ' '
      def_path         = p_file
      mask             = ',*.xlsx.'
      mode             = 'O'
      title            = 'Arquivo a importar'
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 01
      no_batch         = 02
      selection_cancel = 03
      selection_error  = 04.

*----------------------------------------------------------------------*
* START OF SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF ( p_import IS NOT INITIAL ) AND ( p_file IS INITIAL ).
    MESSAGE i000(z01) WITH 'Informe o arquivo a processar!'.
    RETURN.
  ENDIF.

  IF ( x_imob = 'X' ).
    IF ( p_file IS INITIAL ).
      MESSAGE i000(z01) WITH 'Informe o arquivo a processar!'.
      RETURN.
    ENDIF.
    IF p_afabe IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe a área do imobilizado!'.
      RETURN.
    ENDIF.

    IF p_gjahri IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe exercicio do imobilizado!'.
      RETURN.
    ENDIF.
  ENDIF.

  IF ( x_imob_e = 'X' ).
    IF p_bukrse IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe a empresa do imobilizado ENCERRA !'.
      RETURN.
    ENDIF.

    IF p_gjahre IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe ano encerramento imobilizado ENCERRA!'.
      RETURN.
    ENDIF.
  ENDIF.

  IF ( x_soc = 'X' ).
    IF p_bukrs IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe a Empresa !'.
      RETURN.
    ENDIF.
    IF p_gjahr IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe o Ano !'.
      RETURN.
    ENDIF.
    IF p_belnr IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe o Documento !'.
      RETURN.
    ENDIF.
    IF p_buzei IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe o Buzei !'.
      RETURN.
    ENDIF.
    IF p_vbund IS INITIAL.
      MESSAGE i000(z01) WITH 'Informe a Soc.parc.negócios !'.
      RETURN.
    ENDIF.
  ENDIF.

  IF  x_bsad = 'X'.
    PERFORM: form_atualiza_bsad.
  ELSEIF x_bsid = 'X'.
    PERFORM: form_atualiza_bsid.
  ELSEIF x_zib = 'X'.
    PERFORM: form_atualiza_zib.
  ELSEIF x_bseg = 'X'.
    PERFORM: form_atualiza_bseg.
  ELSEIF x_regu = 'X'.
    PERFORM: form_atualiza_regu.
  ELSEIF x_imob = 'X'.
    PERFORM: form_atualiza_imob.
  ELSEIF x_imob_e = 'X'.
    PERFORM: form_atualiza_imob_e.
  ELSEIF x_soc = 'X'.
    PERFORM: form_atualiza_bseg_soc_parc.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  form_atualiza_bsad
*&---------------------------------------------------------------------*
FORM form_atualiza_bsad .

  SELECT *
  FROM bsad
  INTO TABLE it_bsad
WHERE bukrs IN p_bukrs
  AND gjahr IN p_gjahr
  AND belnr IN p_belnr
  AND buzei IN p_buzei.

  IF p_import IS NOT INITIAL. "Processa Planilha
    REFRESH: it_bsad.
    PERFORM  form_proc_excel USING 'BSAD'.
  ENDIF.

  IF it_bsad IS NOT INITIAL.
    LOOP AT it_bsad INTO wk_bsad.

      IF ( p_import IS INITIAL ).
        MOVE: p_vbel2-low   TO wk_bsad-vbel2,
              '0010'        TO wk_bsad-vpos2,
              p_cond-low    TO wk_bsad-zterm,
              p_buzei       TO wk_bsad-buzei.
      ENDIF.

      MODIFY bsad FROM wk_bsad.
    ENDLOOP.
  ENDIF.
  MESSAGE i000(z01) WITH 'Processamento concluido com sucesso!'.

  CLEAR: wk_bsad,
         it_bsad,
         p_bukrs,
         p_gjahr,
         p_belnr,
         p_vbel2.


ENDFORM.                    "form_atualiza_bsad

*&---------------------------------------------------------------------*
*&      Form  form_atualiza_bsid
*&---------------------------------------------------------------------*
FORM form_atualiza_bsid .

  SELECT *
  FROM bsid
  INTO TABLE it_bsid
WHERE bukrs IN p_bukrs
  AND gjahr IN p_gjahr
  AND belnr IN p_belnr
  AND buzei IN p_buzei.

  IF p_import IS NOT INITIAL. "Processa Planilha
    REFRESH: it_bsid.
    PERFORM  form_proc_excel USING 'BSID'.
  ENDIF.

  IF it_bsid IS NOT INITIAL.
    LOOP AT it_bsid INTO wk_bsid.

      IF ( p_import IS INITIAL ).
        MOVE: p_vbel2-low   TO wk_bsid-vbel2,
              '0010'        TO wk_bsid-posn2,
              p_cond-low    TO wk_bsid-zterm,
              p_buzei       TO wk_bsid-buzei.
      ENDIF.

      MODIFY bsid FROM wk_bsid.
    ENDLOOP.
  ENDIF.
  MESSAGE i000(z01) WITH 'Processamento concluido com sucesso!'.

  CLEAR: wk_bsid,
         it_bsid,
         p_bukrs,
         p_gjahr,
         p_belnr,
         p_vbel2.


ENDFORM.                    "form_atualiza_bsid
*&---------------------------------------------------------------------*
*&      Form  FORM_ATUALIZA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_atualiza_zib .
  DATA: vlinhas TYPE i.

  SELECT COUNT(*)
    INTO vlinhas
    FROM  zib_contabil
  WHERE gjahr = 2012 AND monat = 12 AND budat = '31.12.2012' AND ( bukrs = '0015' OR bukrs = '0018' ) AND obj_key LIKE 'FIAA%'.

  IF vlinhas = 5183.
    DELETE FROM zib_contabil
    WHERE gjahr = 2012 AND monat = 12 AND budat = '31.12.2012' AND ( bukrs = '0015' OR bukrs = '0018' ) AND obj_key LIKE 'FIAA%'.
    MESSAGE i000(z01) WITH 'Apagado 31.12.2012 gerado errado!'.
  ELSE.
    MESSAGE i000(z01) WITH 'Não existe dados para apagar em 31.12.2012!'.
  ENDIF.

  SELECT COUNT(*)
   INTO vlinhas
   FROM  zib_contabil
 WHERE gjahr = 2012 AND monat = 12 AND budat = '00.00.0000' AND ( bukrs = '0015' OR bukrs = '0018' OR bukrs = '0035' ) AND obj_key LIKE 'FIAA%'.

  IF  vlinhas = 14730.
    UPDATE zib_contabil SET budat = '31.12.2012'
                            bldat = '31.12.2012'
                            rg_atualizado = 'N'
   WHERE gjahr = 2012 AND monat = 12 AND budat = '00.00.0000' AND ( bukrs = '0015' OR bukrs = '0018' OR bukrs = '0035' ) AND obj_key LIKE 'FIAA%'.
    MESSAGE i000(z01) WITH 'Alterado datas para 31.12.2012!'.
  ELSE.
    MESSAGE i000(z01) WITH 'Condição não encontrada para alterar data para 31.12.2012!'.
  ENDIF.
ENDFORM.                    " FORM_ATUALIZA_ZIB

FORM form_atualiza_bseg.

  SELECT *
   FROM bseg
   INTO TABLE it_bseg
 WHERE bukrs IN p_bukrs
   AND gjahr IN p_gjahr
   AND belnr IN p_belnr
   AND buzei IN p_buzei.

  IF p_import IS NOT INITIAL. "Processa Planilha
    REFRESH: it_bseg.
    PERFORM  form_proc_excel USING 'BSEG'.
  ENDIF.

  IF it_bseg IS NOT INITIAL.
    LOOP AT it_bseg INTO wk_bseg.

      IF ( p_import IS INITIAL ).
        MOVE: p_vbel2-low   TO wk_bseg-vbel2,
              '0010'        TO wk_bseg-posn2,
              p_cond-low    TO wk_bseg-zterm,
              p_buzei       TO wk_bseg-buzei.
      ENDIF.

      MODIFY bseg FROM wk_bseg.
    ENDLOOP.
  ENDIF.
  MESSAGE i000(z01) WITH 'Processamento concluido com sucesso!'.

  CLEAR: wk_bseg,
         it_bseg,
         p_bukrs,
         p_gjahr,
         p_belnr,
         p_vbel2.


ENDFORM.


FORM form_proc_excel USING p_tipo.

  TYPES: BEGIN OF ty_doc,
           bukrs TYPE bsid-bukrs,
           gjahr TYPE bsid-gjahr,
           belnr TYPE bsid-belnr,
           buzei TYPE bsid-buzei,
           vbel2 TYPE bsid-vbel2,
         END OF ty_doc.

  DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
        wl_planilha LIKE alsmex_tabline,
        wl_doc      TYPE ty_doc,
        wl_vbak     TYPE vbak.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Processando'.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 55
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CHECK gt_planilha[] IS NOT INITIAL.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR: wl_doc.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wl_doc-bukrs   = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-bukrs
          IMPORTING
            output = wl_doc-bukrs.

      WHEN 2.
        wl_doc-gjahr   = wl_planilha-value.
      WHEN 3.
        wl_doc-belnr   = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-belnr
          IMPORTING
            output = wl_doc-belnr.
      WHEN 4.
        wl_doc-buzei   = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-buzei
          IMPORTING
            output = wl_doc-buzei.
      WHEN 5.
        wl_doc-vbel2   = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-vbel2
          IMPORTING
            output = wl_doc-vbel2.

        IF wl_doc-vbel2 IS NOT INITIAL.
          SELECT SINGLE *
            FROM vbak INTO wl_vbak
           WHERE vbeln = wl_doc-vbel2.

          IF sy-subrc NE 0 .
            CLEAR: wl_doc-vbel2.
          ENDIF.
        ENDIF.

    ENDCASE.

    AT END OF row.

      IF ( wl_doc-bukrs IS NOT INITIAL ) AND
         ( wl_doc-gjahr IS NOT INITIAL ) AND
         ( wl_doc-belnr IS NOT INITIAL ) AND
         ( wl_doc-buzei IS NOT INITIAL ) AND
         ( wl_doc-vbel2 IS NOT INITIAL ).

        CASE p_tipo.
          WHEN 'BSID'.
            SELECT SINGLE *
              FROM bsid INTO wk_bsid
             WHERE bukrs EQ wl_doc-bukrs
               AND gjahr EQ wl_doc-gjahr
               AND belnr EQ wl_doc-belnr
               AND buzei EQ wl_doc-buzei.

            IF sy-subrc = 0.
              wk_bsid-vbel2 = wl_doc-vbel2.
              wk_bsid-posn2 = '0010'.
              APPEND wk_bsid TO it_bsid.
            ENDIF.

          WHEN 'BSAD'.
            SELECT SINGLE *
              FROM bsad INTO wk_bsad
             WHERE bukrs EQ wl_doc-bukrs
               AND gjahr EQ wl_doc-gjahr
               AND belnr EQ wl_doc-belnr
               AND buzei EQ wl_doc-buzei.

            IF sy-subrc = 0.
              wk_bsad-vbel2 = wl_doc-vbel2.
              wk_bsad-posn2 = '0010'.
              APPEND wk_bsad TO it_bsad.
            ENDIF.
          WHEN 'BSEG'.
            SELECT SINGLE *
              FROM bseg INTO wk_bseg
             WHERE bukrs EQ wl_doc-bukrs
               AND gjahr EQ wl_doc-gjahr
               AND belnr EQ wl_doc-belnr
               AND buzei EQ wl_doc-buzei.

            IF sy-subrc = 0.
              wk_bseg-vbel2 = wl_doc-vbel2.
              wk_bseg-posn2 = '0010'.
              APPEND wk_bseg TO it_bseg.
            ENDIF.
        ENDCASE.

      ENDIF.

    ENDAT.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_ATUALIZA_REGU
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_atualiza_regu .

  DATA vmessa(50).
  DATA w_answer(1).
  DATA w_reguh(4).
  DATA w_regup(4).

  SELECT COUNT(*)
    FROM regup
    INTO @DATA(_cregup)
   WHERE laufd = @p_laufd
  AND   laufi = @p_laufi.

  w_regup = _cregup.
  SELECT COUNT(*)
     FROM reguh
     INTO @DATA(_creguh)
    WHERE laufd = @p_laufd
   AND   laufi = @p_laufi.

  w_reguh = _creguh.
  CONCATENATE 'Registros REGHU' w_reguh 'REGUP' w_regup INTO vmessa SEPARATED BY space.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question         = vmessa
      text_button_1         = 'Sim'(100)
      icon_button_1         = 'ICON_OKAY '
      text_button_2         = 'Não'(101)
      icon_button_2         = 'ICON_CANCEL'
      default_button        = '1'
      display_cancel_button = ' '
      start_column          = 25
      start_row             = 6
    IMPORTING
      answer                = w_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  IF w_answer = '1'. "não
    DELETE FROM regup
    WHERE laufd = p_laufd
    AND   laufi = p_laufi.

    DELETE FROM reguh
    WHERE laufd = p_laufd
    AND   laufi = p_laufi.

    COMMIT WORK.

    MESSAGE 'Executado eliminação REGUH/REGUP' TYPE 'I'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_ATUALIZA_IMOB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_atualiza_imob .
  TYPES: BEGIN OF ty_doc,
           anln1   TYPE anlc-anln1,
           anln2   TYPE anlc-anln2,
           bukrs_n TYPE anlc-bukrs,
           bukrs_a TYPE anlc-bukrs,
         END OF ty_doc.

  DATA: gt_planilha LIKE STANDARD TABLE OF alsmex_tabline,
        wl_planilha LIKE alsmex_tabline,
        wl_doc      TYPE ty_doc,
        v_nafaz     TYPE anlp-nafaz,
        v_nafap     TYPE anlp-nafap.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = 'Processando'.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 5
      i_end_row               = 10000
    TABLES
      intern                  = gt_planilha
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  CHECK gt_planilha[] IS NOT INITIAL.

  LOOP AT gt_planilha INTO wl_planilha.
    AT NEW row.
      CLEAR: wl_doc.
    ENDAT.

    IF wl_planilha-value(1) = space.
      SHIFT wl_planilha-value LEFT DELETING LEADING space.
    ENDIF.

    CASE wl_planilha-col.
      WHEN 1.
        wl_doc-anln1   = wl_planilha-value.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-anln1
          IMPORTING
            output = wl_doc-anln1.

      WHEN 2.
        wl_doc-anln2   = wl_planilha-value.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-anln2
          IMPORTING
            output = wl_doc-anln2.
      WHEN 3.
        wl_doc-bukrs_n   = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-bukrs_n
          IMPORTING
            output = wl_doc-bukrs_n.
      WHEN 4.
        wl_doc-bukrs_a   = wl_planilha-value.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wl_doc-bukrs_a
          IMPORTING
            output = wl_doc-bukrs_a.

    ENDCASE.

    AT END OF row.
      SELECT SINGLE *
        FROM anla
        INTO @DATA(w_anla)
        WHERE bukrs = @wl_doc-bukrs_n
        AND   anln1 = @wl_doc-anln1
        AND   anln2 = @wl_doc-anln2.
      "
      IF sy-subrc = 0.
        SELECT SINGLE *
        FROM anlc
        INTO @DATA(w_anlc_ant)
        WHERE bukrs = @wl_doc-bukrs_a
        AND   anln1 = @w_anla-aibn1
        AND   anln2 = @w_anla-aibn2
        AND   afabe = @p_afabe
        AND   gjahr = @p_gjahri.
        IF sy-subrc = 0.
          v_nafap = w_anlc_ant-nafap.
          SELECT SINGLE *
            FROM anlp
            INTO @DATA(w_anlp_nov)
            WHERE bukrs   = @w_anla-bukrs
            AND   gjahr   = @p_gjahri
            AND   peraf   = '012'
            AND   anln1  	=	@w_anla-anln1
            AND   anln2	  =	@w_anla-anln2
            AND   afaber  = @w_anlc_ant-afabe.
          IF sy-subrc = 0.
            v_nafaz = ( w_anlc_ant-nafap - w_anlp_nov-nafag ).
            UPDATE anlp SET nafap	=	w_anlc_ant-nafap
                            nafaz	=	v_nafaz
            WHERE bukrs   = w_anla-bukrs
            AND   gjahr   = p_gjahri
            AND   peraf   = '012'
            AND   anln1  	=	w_anla-anln1
            AND   anln2	  =	w_anla-anln2
            AND   afaber  = w_anlc_ant-afabe.
            "
            UPDATE anlc SET nafap = v_nafap
                            nafag = v_nafap
            WHERE bukrs	=	w_anla-bukrs
            AND  anln1  = w_anla-anln1
            AND  anln2  = w_anla-anln2
            AND  afabe  = w_anlc_ant-afabe
            AND  gjahr  = p_gjahri.
            "
            COMMIT WORK.
          ENDIF.
        ENDIF.
      ENDIF.



    ENDAT.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_ATUALIZA_IMOB_E
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_atualiza_imob_e .
  UPDATE t093b SET abgja = p_gjahre
  WHERE bukrs = p_bukrse.
  COMMIT WORK.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FORM_ATUALIZA_BSEG_SOC_PARC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM form_atualiza_bseg_soc_parc .

  SELECT *
     FROM bseg
     INTO TABLE it_bseg
   WHERE bukrs IN p_bukrs
     AND gjahr IN p_gjahr
     AND belnr IN p_belnr
     AND buzei IN p_buzei.

  IF it_bseg IS NOT INITIAL.

    LOOP AT it_bseg INTO wk_bseg.
      wk_bseg-vbund = p_vbund-low.
      MODIFY bseg FROM wk_bseg.
    ENDLOOP.

    MESSAGE i000(z01) WITH 'Processamento concluido com sucesso!'.
  ELSE.
    MESSAGE i000(z01) WITH 'Nenhum documento não encontrado !'.
  ENDIF.

  CLEAR: wk_bseg,
         it_bseg,
         p_bukrs,
         p_gjahr,
         p_belnr,
         p_buzei,
         p_vbund.

ENDFORM.
