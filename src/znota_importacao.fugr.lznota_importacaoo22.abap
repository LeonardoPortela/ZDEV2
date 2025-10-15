*----------------------------------------------------------------------*
***INCLUDE LZNOTA_IMPORTACAOO22 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_2002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2002 OUTPUT.

  CLEAR: it_fcode_di.

  CASE ok_di_altera.
    WHEN space.
      wa_fcode_di = ok_di_gravar.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_cancela.
      APPEND wa_fcode_di TO it_fcode_di.
    WHEN c_x.
      wa_fcode_di = ok_di_inserir.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_editar.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_excluir.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_sair.
      APPEND wa_fcode_di TO it_fcode_di.
  ENDCASE.

  SET PF-STATUS 'PFDI' EXCLUDING it_fcode_di.
  SET TITLEBAR  'TLDI'.

ENDMODULE.                 " STATUS_2002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2002 INPUT.

  DATA: vg_j_1bitmnum TYPE j_1bitmnum.

  CASE              ok_code_di.
    WHEN ok_di_inserir.
      znota_import-docnum = vg_docnum.
      znota_import-itmnum = vg_itmnum.
      ok_di_altera = c_x.
    WHEN ok_di_editar .
      ok_di_altera = c_x.
    WHEN ok_di_excluir.
      DELETE FROM znota_import_ad
       WHERE docnum  EQ znota_import-docnum
         AND itmnum  EQ znota_import-itmnum
         AND itdidoc EQ znota_import-itdidoc.
      DELETE FROM znota_import
       WHERE docnum  EQ znota_import-docnum
         AND itmnum  EQ znota_import-itmnum
         AND itdidoc EQ znota_import-itdidoc.
      COMMIT  WORK.
      PERFORM pesquisa_di USING vg_docnum vg_itmnum.
      IF NOT it_znota_import[] IS INITIAL.
        READ TABLE it_znota_import INTO znota_import INDEX 1.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN ok_di_gravar .

      IF znota_import-tpviatransp <> '1'.
        IF znota_import-vafrmm <> 0.
          MESSAGE s899(sd) WITH 'Valor AFRM só é informado p/ via de Transporte Marítima!'.
          CLEAR znota_import-vafrmm.
          RETURN.
        ENDIF.
      ENDIF.

      IF znota_import-itdidoc IS INITIAL.
        SELECT MAX( itdidoc ) INTO vg_j_1bitmnum
          FROM znota_import
         WHERE docnum EQ vg_docnum
           AND itmnum EQ vg_itmnum.

        IF sy-subrc IS INITIAL.
          znota_import-itdidoc = vg_j_1bitmnum + 1.
        ELSE.
          znota_import-itdidoc = 1.
        ENDIF.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ZNRDI_INPUT'
        EXPORTING
          input  = znota_import-ndi
        IMPORTING
          output = znota_import-ndi.

      "********************************************************************** start "163040 CS2024001206 Dados importação em massa PSA

      FREE: lr_nunm.

      SELECT
      'I' AS sign,
      'EQ' AS option,
      itmnum AS low
      FROM znota_import WHERE docnum = @znota_import-docnum INTO TABLE @lr_nunm.

      IF lr_nunm IS NOT INITIAL.
        SORT lr_nunm.
        DELETE ADJACENT DUPLICATES FROM lr_nunm.
        FREE: it_j_1bnflin_aux.
        it_j_1bnflin_aux = it_j_1bnflin[].
        SORT it_j_1bnflin_aux.
        DELETE it_j_1bnflin_aux WHERE itmnum IN lr_nunm.
      ELSE.
        it_j_1bnflin_aux = it_j_1bnflin[].
      ENDIF.

      CLEAR: qtdlines.
      qtdLines = lines( it_j_1bnflin_aux[] ).

      IF qtdLines > 1.
        DATA: ls_answer TYPE answer.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Lançamento em Massa'
            text_question         = 'Deseja replicar os dados para todos os itens?'
            text_button_1         = 'Sim'
            icon_button_1         = CONV icon-name( 'ICON_OKAY' )
            text_button_2         = 'Não'
            icon_button_2         = CONV icon-name( 'ICON_CANCEL' )
            default_button        = '1'
            display_cancel_button = space
            popup_type            = 'ICON_MESSAGE_WARNING'
          IMPORTING
            answer                = ls_answer.


        IF sy-subrc = 0 AND ls_answer = 1.

          LOOP AT it_j_1bnflin_aux ASSIGNING FIELD-SYMBOL(<fs_J_1BNFLIN>).
            READ TABLE it_znota_import[] ASSIGNING FIELD-SYMBOL(<znota_import>) WITH KEY itmnum = <fs_J_1BNFLIN>-itmnum.
            IF sy-subrc <> 0.
              znota_import-itmnum = <fs_J_1BNFLIN>-itmnum.
              "APPEND znota_import TO it_znota_import[].
              MODIFY znota_import.
              COMMIT WORK.
            ENDIF.
          ENDLOOP.

        ELSE.

          MODIFY znota_import.
          COMMIT WORK.

          PERFORM pesquisa_di USING vg_docnum vg_itmnum.

        ENDIF.

        ok_di_altera = space.

        PERFORM update_dados_di. "CS2022000572 #79084 FF   06.01.2023

      ELSE.

        MODIFY znota_import.
        COMMIT WORK.

        PERFORM pesquisa_di USING vg_docnum vg_itmnum.
        ok_di_altera = space.

        PERFORM update_dados_di. "CS2022000572 #79084 FF   06.01.2023

      ENDIF.

      "********************************************************************** end "163040 CS2024001206 Dados importação em massa PSA

*      A tag 'vAFRMM' deve ser informada no caso da via de transporte marítima.
*      IF znota_import-tpviatransp <> '1'.
*        IF znota_import-vafrmm <> 0.
*          MESSAGE s899(sd) WITH 'Valor AFRM só é informado p/ via de Transporte Marítima!'.
*          CLEAR znota_import-vafrmm.
*          RETURN.
*        ENDIF.
*      ENDIF.
*
*      IF znota_import-itdidoc IS INITIAL.
*        SELECT MAX( itdidoc ) INTO vg_j_1bitmnum
*          FROM znota_import
*         WHERE docnum EQ vg_docnum
*           AND itmnum EQ vg_itmnum.
*
*        IF sy-subrc IS INITIAL.
*          znota_import-itdidoc = vg_j_1bitmnum + 1.
*        ELSE.
*          znota_import-itdidoc = 1.
*        ENDIF.
*      ENDIF.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ZNRDI_INPUT'
*        EXPORTING
*          input  = znota_import-ndi
*        IMPORTING
*          output = znota_import-ndi.
*
*      MODIFY znota_import.
*      COMMIT WORK.
*      PERFORM pesquisa_di USING vg_docnum vg_itmnum.
*
*      ok_di_altera = space.
*
*      PERFORM update_dados_di. "CS2022000572 #79084 FF   06.01.2023

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2002  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2002_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_2002_exit INPUT.
  CASE ok_code_di.
    WHEN ok_di_cancela.
      CLEAR: ok_di_altera, znota_import.
      PERFORM pesquisa_di USING vg_docnum vg_itmnum.
      IF NOT it_znota_import[] IS INITIAL.
        READ TABLE it_znota_import INTO znota_import INDEX 1.
        LEAVE TO SCREEN 2002.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN ok_di_sair.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_2002_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_2002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE visibilidade_2002 OUTPUT.

  LOOP AT SCREEN.
    IF ok_di_altera IS INITIAL.
      screen-output = '1'.
      screen-input  = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " VISIBILIDADE_2002  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  UPDATE_DADOS_DI
*&---------------------------------------------------------------------*
FORM update_dados_di.

  DATA: wa_doc_header LIKE  j_1bnfdoc,
        zdoc_texts    TYPE REF TO if_logbr_nf_texts_data, "Inicio ajuste melhoria USER STORY 163037 / AOENNING
        it_doc_texts  TYPE  logbr_nf_text_tt.             "Inicio ajuste melhoria USER STORY 163037 / AOENNING


  DATA:
    it_doc_partner    TYPE TABLE OF j_1bnfnad,
    it_doc_item	      TYPE TABLE OF j_1bnflin,
    it_doc_item_tax	  TYPE TABLE OF	j_1bnfstx,
    it_doc_header_msg	TYPE TABLE OF	j_1bnfftx,
    it_doc_refer_msg  TYPE TABLE OF j_1bnfref,
    it_doc_import_di  TYPE TABLE OF j_1bnfimport_di,
    it_doc_import_adi TYPE TABLE OF j_1bnfimport_adi,
    wa_doc_import_di  TYPE j_1bnfimport_di.

  FIELD-SYMBOLS: <fs_di> TYPE  j_1bnfimport_di .


  CALL FUNCTION 'J_1B_NF_DOCUMENT_READ'
    EXPORTING
      doc_number         = vg_docnum
    IMPORTING
      doc_header         = wa_doc_header
      doc_texts          = zdoc_texts  "Inicio ajuste melhoria USER STORY 163037 / AOENNING
    TABLES
      doc_partner        = it_doc_partner
      doc_item           = it_doc_item
      doc_item_tax       = it_doc_item_tax
      doc_header_msg     = it_doc_header_msg
      doc_refer_msg      = it_doc_refer_msg
      doc_import_di      = it_doc_import_di
      doc_import_adi     = it_doc_import_adi
    EXCEPTIONS
      document_not_found = 1
      docum_lock         = 2
      OTHERS             = 3.



*  IF sy-subrc = 0.
*    DESCRIBE TABLE it_doc_import_di.
*    IF sy-tfill EQ 0.
*     " APPEND INITIAL LINE TO it_doc_import_di.
*      " READ TABLE it_doc_import_di  ASSIGNING FIELD-SYMBOL(<fs_di>) INDEX 1 .
*      ASSIGN wa_doc_import_di TO <fs_di>.
*    ELSE.
*
*    READ TABLE it_doc_import_di WITH KEY
*      mandt = znota_import-mandt
*      docnum   = znota_import-docnum
*      ndi      = znota_import-ndi
*    BINARY SEARCH  ASSIGNING <fs_di> .
*   " ASSIGN it_doc_import_di TO <fs_di>.
*   ENDIF.

  IF sy-subrc = 0.

    READ TABLE it_doc_import_di WITH KEY
      mandt = znota_import-mandt
      docnum   = znota_import-docnum
      ndi      = znota_import-ndi
    BINARY SEARCH  ASSIGNING <fs_di> .

    IF sy-subrc NE 0.
      ASSIGN wa_doc_import_di TO <fs_di>.
    ENDIF.
    "
    " LOOP AT it_doc_import_di ASSIGNING FIELD-SYMBOL(<fs_di>).

    <fs_di>-docnum            = vg_docnum.
    <fs_di>-ndi               = znota_import-ndi.
    <fs_di>-ddi               = znota_import-ddi.
    <fs_di>-xlocdesemb        = znota_import-xlocdesemb.
    <fs_di>-ufdesemb          = znota_import-ufdesemb.
    <fs_di>-ddesemb           = znota_import-ddesemb.
    <fs_di>-cexportador       = znota_import-cexportador.
    <fs_di>-transport_mode    = znota_import-tpviatransp.
    <fs_di>-intermediate_mode = znota_import-tpintermedio.
    <fs_di>-cod_doc_imp       = '1'.

    APPEND <fs_di> TO it_doc_import_di.
    " ENDLOOP.

*>>>>>>>>Inicio ajuste melhoria USER STORY 163037 / AOENNING <<<<<<<<<<<<<<<*
    FREE: it_doc_texts.
    LOOP AT zdoc_texts->get_text_table( ) ASSIGNING FIELD-SYMBOL(<nf_texts_line>).

      APPEND VALUE #( docnum = <nf_texts_line>-docnum
                        itmnum = <nf_texts_line>-itmnum
                          type = <nf_texts_line>-type
                       counter = <nf_texts_line>-counter
                          text = <nf_texts_line>-text
                        textid = <nf_texts_line>-textid
                    ) TO it_doc_texts.
    ENDLOOP.
*>>>>>>>>Inicio ajuste melhoria USER STORY 163037 / AOENNING <<<<<<<<<<<<<<<*

    CALL FUNCTION 'J_1B_NF_DOCUMENT_UPDATE'
      EXPORTING
        doc_number            = vg_docnum
        doc_header            = wa_doc_header
        doc_texts             = it_doc_texts "Inicio ajuste melhoria USER STORY 163037 / AOENNING
      TABLES
        doc_partner           = it_doc_partner
        doc_item              = it_doc_item
        doc_item_tax          = it_doc_item_tax
        doc_header_msg        = it_doc_header_msg
        doc_refer_msg         = it_doc_refer_msg
        doc_import_di         = it_doc_import_di
        doc_import_adi        = it_doc_import_adi
      EXCEPTIONS
        document_not_found    = 1
        update_problem        = 2
        doc_number_is_initial = 3
        OTHERS                = 4.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.

ENDFORM.

FORM alv_lm."163040 CS2024001206 Dados importação em massa PSA

  FREE: lr_nunm.
  CLEAR:wa_J_1BNFLIN.
  READ TABLE it_j_1bnflin INTO wa_J_1BNFLIN INDEX 1.

  SELECT
  'I' AS sign,
  'EQ' AS option,
  itmnum AS low
  FROM znota_import WHERE docnum = @wa_J_1BNFLIN-docnum INTO TABLE @lr_nunm.

  IF sy-subrc = 0.

    SORT lr_nunm.

    DELETE ADJACENT DUPLICATES FROM lr_nunm.

    FREE: it_j_1bnflin_aux.
    it_j_1bnflin_aux = it_j_1bnflin[].
    SORT it_j_1bnflin_aux.
    DELETE it_j_1bnflin_aux WHERE itmnum IN lr_nunm.

    CLEAR: qtdlines.
    qtdLines = lines( it_j_1bnflin_aux[] ).

    IF qtdLines = 0.

      DATA:it_saida2 TYPE TABLE OF znota_import_ad,  " Replace with your table
           wa_saida2 TYPE znota_import_ad.

      CLEAR: wa_saida2.
      FREE: it_saida2.

      SELECT * FROM znota_import  WHERE docnum = @wa_J_1BNFLIN-docnum INTO TABLE @DATA(it_dados).


      LOOP AT it_dados INTO DATA(ls_dados).

        wa_saida2-docnum = ls_dados-docnum.
        wa_saida2-itmnum = ls_dados-itmnum.
        wa_saida2-itdidoc = ls_dados-itdidoc.
        "wa_saida2-nr_adicao = ls_dados-itdidoc.
        "wa_saida2-nr_seq_adicao = ls_dados-itdidoc.
        wa_saida2-mandt = sy-mandt.
        APPEND wa_saida2 TO it_saida2.
        CLEAR: wa_saida2.
      ENDLOOP.

      "send LZNOTA_IMPORTACAOO22 to ZSAPLZNOTA_IMPLM
      EXPORT it_saida2 = it_saida2 TO MEMORY ID 'ZSAPLZNOTA_IMPLM'.
      SUBMIT zsaplznota_implm AND RETURN .

    ELSE.

      MESSAGE 'É todos os itens DI!' TYPE 'I' DISPLAY LIKE 'I'.
      EXIT.

    ENDIF.


  ELSE.
    MESSAGE 'É necessário preencher a DI!' TYPE 'I' DISPLAY LIKE 'I'.
    EXIT.
  ENDIF.
ENDFORM.
