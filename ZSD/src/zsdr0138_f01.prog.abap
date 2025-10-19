*----------------------------------------------------------------------*
***INCLUDE ZSDR0138_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  DATA_SELECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM data_selection.

  SELECT *
    FROM vbrk
    INTO TABLE gt_vbrk
    WHERE vbeln = p_vbeln.

  IF sy-subrc EQ 0.

    READ TABLE gt_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>) INDEX 1.
    IF sy-subrc EQ 0.
      vg_lifnr = <fs_vbrk>-bupla.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = vg_lifnr
        IMPORTING
          output = vg_lifnr.

      IF <fs_vbrk>-fkart NE 'ZPAR'.
        MESSAGE i001(zsd) WITH text-003. "Apenas Tipo de Documento ZPAR é permitido !
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.

    SELECT *
      FROM vbrp
      INTO TABLE gt_vbrp
      WHERE vbeln = p_vbeln.

    SELECT *
      FROM t001
      INTO TABLE gt_t001
      FOR ALL ENTRIES IN gt_vbrk
      WHERE bukrs = gt_vbrk-vkorg.

    IF sy-subrc EQ 0.

      SELECT *
        FROM adrc
        INTO TABLE gt_adrc
        FOR ALL ENTRIES IN gt_t001
        WHERE addrnumber = gt_t001-adrnr.

    ENDIF.

    SELECT *
      FROM kna1
      INTO TABLE gt_kna1
      FOR ALL ENTRIES IN gt_vbrk
      WHERE kunnr = gt_vbrk-kunag.

    SELECT *
      FROM zsdt0001
      INTO TABLE gt_zsdt0001
      FOR ALL ENTRIES IN gt_vbrk
      WHERE fatura_prod = gt_vbrk-vbeln.

    IF sy-subrc EQ 0.

      SELECT *
        FROM lfa1
        INTO TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_zsdt0001
        WHERE lifnr = gt_zsdt0001-agente_frete.

      SELECT *
        FROM lfa1
        APPENDING TABLE gt_lfa1
        FOR ALL ENTRIES IN gt_zsdt0001
        WHERE lifnr = gt_zsdt0001-motorista.

    ENDIF.

    SELECT *
      FROM lfa1
      APPENDING TABLE gt_lfa1
      WHERE lifnr = vg_lifnr.

  ELSE.

    MESSAGE i001(zsd) WITH text-002.
    LEAVE LIST-PROCESSING.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM clear .

  REFRESH: gt_vbrk      ,
           gt_vbrp      ,
           gt_t001      ,
           gt_adrc      ,
           gt_kna1      ,
           gt_zsdt0001  ,
           gt_lfa1      .

  CLEAR vg_lifnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  PRINT_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM print_report .

  DATA wa_options       TYPE ssfcompop.
  DATA wa_control       TYPE ssfctrlop.
  DATA wa_fornecedor    TYPE zsds061.
  DATA wa_parceiro      TYPE zsds062.
  DATA wa_transportador TYPE zsds064.
  DATA it_produtos      TYPE zsds063_t.
  DATA lv_vbeln         TYPE vbrk-vbeln.
  DATA vl_formname      TYPE tdsfname.
  DATA vl_name          TYPE rs38l_fnam.
  DATA lv_cgc_company   TYPE j_1bwfield-cgc_compan.
  DATA lv_branch        TYPE j_1bbranch-branch.
  DATA lv_cgc_number    TYPE j_1bwfield-cgc_number.

  vl_formname = 'ZSDF0014'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_formname
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  wa_options-tddest   = 'LOCL'.     "Disposit. saída
  wa_options-tdimmed  = abap_true.  "Saída Imediata
  wa_options-tdnewid  = abap_true.  "Nova Ordem SPOOL
  wa_options-tdcovtitle = 'Partner Note Printing Program'.

  wa_control-no_open  = abap_false.
  wa_control-no_close = abap_true.

  lv_vbeln = p_vbeln.

  READ TABLE gt_vbrk ASSIGNING FIELD-SYMBOL(<fs_vbrk>) INDEX sy-tabix.

  IF sy-subrc EQ 0.

    READ TABLE gt_t001 ASSIGNING FIELD-SYMBOL(<fs_t001>) INDEX sy-tabix.

    IF sy-subrc EQ 0.

*      READ TABLE gt_adrc ASSIGNING FIELD-SYMBOL(<fs_adrc>) WITH KEY addrnumber = <fs_t001>-adrnr.
*
*      IF sy-subrc EQ 0.

      READ TABLE gt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1_forn>) WITH KEY lifnr = vg_lifnr.

      IF sy-subrc EQ 0.

*          CALL FUNCTION 'J_1BREAD_CGC_COMPANY'
*            EXPORTING
*              bukrs       = <fs_vbrk>-vkorg
*            IMPORTING
*              cgc_company = lv_cgc_company
*              branch      = lv_branch
*              cgc_number  = lv_cgc_number.

        wa_fornecedor-butxt       = <fs_lfa1_forn>-name1.
        wa_fornecedor-cnpj        = <fs_lfa1_forn>-stcd1.
        wa_fornecedor-ie          = <fs_lfa1_forn>-stcd2.
        wa_fornecedor-street      = <fs_lfa1_forn>-stras.
        wa_fornecedor-house_num1  = ''.
        wa_fornecedor-city2       = <fs_lfa1_forn>-ort01.
        wa_fornecedor-taxjurcode  = <fs_lfa1_forn>-regio.
        wa_fornecedor-post_code1  = <fs_lfa1_forn>-pstlz.

      ENDIF.

      READ TABLE gt_kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>) WITH KEY kunnr = <fs_vbrk>-kunag.

      IF sy-subrc EQ 0.

        wa_parceiro-name1         = <fs_kna1>-name1.
        wa_parceiro-stcd1         = <fs_kna1>-stcd1.
        wa_parceiro-stcd2         = <fs_kna1>-stcd2.
        wa_parceiro-stcd3         = <fs_kna1>-stcd3.
        wa_parceiro-stras         = <fs_kna1>-stras.
        wa_parceiro-ort02         = <fs_kna1>-ort02.
        wa_parceiro-mcod3         = <fs_kna1>-mcod3.
        wa_parceiro-txjcd         = <fs_kna1>-txjcd.
        wa_parceiro-pstlz         = <fs_kna1>-pstlz.

        READ TABLE gt_zsdt0001 ASSIGNING FIELD-SYMBOL(<fs_zsdt0001>) WITH KEY fatura_prod = <fs_vbrk>-vbeln.

        IF sy-subrc EQ 0.

          READ TABLE gt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1_transp>) WITH KEY lifnr = <fs_zsdt0001>-agente_frete.

          IF sy-subrc = 0.

            wa_transportador-butxt          = <fs_lfa1_transp>-name1.
            wa_transportador-cnpj           = <fs_lfa1_transp>-stcd1.

          ENDIF.

          READ TABLE gt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1_motorista>) WITH KEY lifnr = <fs_zsdt0001>-motorista.

          IF sy-subrc = 0.
            wa_transportador-name1          = <fs_lfa1_motorista>-name1.
            wa_transportador-cpf            = <fs_lfa1_motorista>-stcd2.
            wa_transportador-contato        = <fs_lfa1_motorista>-telf1.
          ENDIF.

          wa_transportador-placa_cavalo   = <fs_zsdt0001>-placa_cav.
          wa_transportador-placa_carretas = <fs_zsdt0001>-placa_car1 && | - | && <fs_zsdt0001>-placa_car2 && | - | && <fs_zsdt0001>-placa_car3.

        ENDIF.
      ENDIF.
*      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT gt_vbrp ASSIGNING FIELD-SYMBOL(<fs_vbrp>).

    APPEND INITIAL LINE TO it_produtos ASSIGNING FIELD-SYMBOL(<fs_produtos>).
    <fs_produtos>-arktx = <fs_vbrp>-arktx.
    <fs_produtos>-fkimg = <fs_vbrp>-fkimg.
    <fs_produtos>-vrkme = <fs_vbrp>-vrkme.

  ENDLOOP.

  CALL FUNCTION vl_name
    EXPORTING
      fornecedor       = wa_fornecedor
      parceiro         = wa_parceiro
      i_vbeln          = lv_vbeln
      transportador    = wa_transportador
    TABLES
      produtos         = it_produtos
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



ENDFORM.
