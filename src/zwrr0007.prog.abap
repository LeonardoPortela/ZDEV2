*&---------------------------------------------------------------------*
*& Report  ZWRR0007
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwrr0007.

TABLES: zfiwrt0021.


TYPES: BEGIN OF ty_zfiwrt0021,
         contrato          TYPE zfiwrt0021-contrato,
         bukrs             TYPE zfiwrt0021-bukrs,
         desc_empresa      TYPE t001-butxt,
         branch            TYPE zfiwrt0021-branch,
         ano               TYPE zfiwrt0021-ano,
         desc_local        TYPE j_1bbranch-name,
         data_inicio       TYPE zfiwrt0021-data_inicio,
         data_final        TYPE zfiwrt0021-data_final,
         kunnr             TYPE zfiwrt0021-kunnr,
         desc_cliente      TYPE kna1-name1,
         banco             TYPE zfiwrt0021-banco,
         agencia_banc      TYPE zfiwrt0021-agencia_banc,
         conta_banc        TYPE zfiwrt0021-conta_banc,
         vencimento        TYPE zfiwrt0021-vencimento,
         vencimento_f      TYPE zfiwrt0021-vencimento_f,
         texto_nota        TYPE zfiwrt0021-texto_nota,
         operacao          TYPE zfiwrt0021-operacao,
         desc_operacao     TYPE zfiwrt0001-descricao,
         matnr             TYPE zfiwrt0021-matnr,
         desc_material     TYPE makt-maktx,
         ger_autom         TYPE zfiwrt0021-ger_autom,
         tipo              TYPE zfiwrt0021-tipo,
         desc_tipo(25)     TYPE c,
         montante01        TYPE zfiwrt0021-montante01,
         tarifa01          TYPE zfiwrt0021-tarifa01,
         montante02        TYPE zfiwrt0021-montante02,
         tarifa02          TYPE zfiwrt0021-tarifa02,
         montante03        TYPE zfiwrt0021-montante03,
         tarifa03          TYPE zfiwrt0021-tarifa03,
         montante04        TYPE zfiwrt0021-montante04,
         tarifa04          TYPE zfiwrt0021-tarifa04,
         montante05        TYPE zfiwrt0021-montante05,
         tarifa05          TYPE zfiwrt0021-tarifa05,
         montante06        TYPE zfiwrt0021-montante06,
         tarifa06          TYPE zfiwrt0021-tarifa06,
         montante07        TYPE zfiwrt0021-montante07,
         tarifa07          TYPE zfiwrt0021-tarifa07,
         montante08        TYPE zfiwrt0021-montante08,
         tarifa08          TYPE zfiwrt0021-tarifa08,
         montante09        TYPE zfiwrt0021-montante09,
         tarifa09          TYPE zfiwrt0021-tarifa09,
         montante10        TYPE zfiwrt0021-montante10,
         tarifa10          TYPE zfiwrt0021-tarifa10,
         montante11        TYPE zfiwrt0021-montante11,
         tarifa11          TYPE zfiwrt0021-tarifa11,
         montante12        TYPE zfiwrt0021-montante12,
         tarifa12          TYPE zfiwrt0021-tarifa12,
         usuario           TYPE zfiwrt0021-usuario,
         dt_modf           TYPE zfiwrt0021-dt_modf,
         hr_modf           TYPE zfiwrt0021-hr_modf,
         total             TYPE zfiwrt0021-tarifa12,
         pgto_mes_seguinte TYPE zfiwrt0021-pgto_mes_seguinte,
         fatura_atu        TYPE zfiwrt0021-fatura_atu,
         fatura_f          TYPE zfiwrt0021-fatura_f,
         fatura_u          TYPE zfiwrt0021-fatura_u,
         total_montante    TYPE zfiwrt0021-total_montante,
         icon(4),
       END OF ty_zfiwrt0021.


DATA: it_zfiwrt0021 TYPE TABLE OF ty_zfiwrt0021,
      wa_zfiwrt0021 TYPE ty_zfiwrt0021.

DATA: it_zfiwrt0022 TYPE TABLE OF zfiwrt0022,
      wa_zfiwrt0022 TYPE zfiwrt0022.

DATA: it_zfiwrt0022_2 TYPE TABLE OF zfiwrt0022,
      wa_zfiwrt0022_2 TYPE zfiwrt0022,
      tl_docest       TYPE TABLE OF zfiwrs0003 WITH HEADER LINE.

DATA: it_t0021 TYPE TABLE OF zfiwrt0021,
      wa_t0021 TYPE zfiwrt0021.

DATA: it_saida TYPE TABLE OF zfiwrt0021,
      wa_saida TYPE zfiwrt0021.
DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
      tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
      it_text_edit       TYPE REF TO cl_gui_textedit,
      it_text            TYPE TABLE OF zfiwrt0021-texto_nota,
      wa_text            TYPE zfiwrt0021-texto_nota.

DATA: ok-code       TYPE sy-ucomm,
      wg_flag       TYPE c,
      vcont         TYPE c,
      wg_acao(3),
      vg_chamada(1),
      vedit         TYPE c,
      vmode         TYPE i.

"ALV 500

DATA: BEGIN OF tl_zfiw0021 OCCURS 0,
        tipo     TYPE zfiwrt0021-tipo,
        contrato TYPE zfiwrt0021-contrato,
        bukrs    TYPE zfiwrt0021-bukrs,
        branch   TYPE zfiwrt0021-branch,
        kunnr    TYPE zfiwrt0021-kunnr,
        name1    TYPE kna1-name1,
        ano      TYPE zfiwrt0021-ano,
      END OF tl_zfiw0021.

DATA: BEGIN OF tl_banco OCCURS 0,
        banks TYPE lfbk-banks,
        bankl TYPE lfbk-bankl, "banco / agencia
        bkont TYPE lfbk-bkont,
        bankn TYPE lfbk-bankn, "Conta corrente
        banco TYPE zfiwrt0021-banco,
      END OF tl_banco.

DATA: obg_conteiner_obj TYPE REF TO cl_gui_custom_container,
      grid5             TYPE REF TO cl_gui_alv_grid,
      gs_variant_c      TYPE disvariant, tg_fieldcatalog     TYPE lvc_t_fcat,
      wg_fieldcatalog   TYPE lvc_s_fcat,
      wa_layout         TYPE lvc_s_layo,
      wa_stable         TYPE lvc_s_stbl.

DATA: obg_conteiner_ban TYPE REF TO cl_gui_custom_container,
      grid4             TYPE REF TO cl_gui_alv_grid.

*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.
    CLASS-METHODS:
      on_double_click4 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_double_click.
    IF e_row GT 0.
      READ TABLE tl_zfiw0021 INTO tl_zfiw0021 INDEX e_row.
      wa_zfiwrt0021-contrato = tl_zfiw0021-contrato.
      wa_zfiwrt0021-bukrs    = tl_zfiw0021-bukrs.
      wa_zfiwrt0021-branch   = tl_zfiw0021-branch.
      wa_zfiwrt0021-kunnr    = tl_zfiw0021-kunnr.
      wa_zfiwrt0021-ano      = tl_zfiw0021-ano.
      SET SCREEN 0.
      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '/00'
        EXCEPTIONS
          function_not_supported = 1.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
  METHOD on_double_click4.
    IF e_row GT 0.
      READ TABLE tl_banco INTO tl_banco INDEX e_row.
      wa_zfiwrt0021-banco         = tl_banco-banco.
      CONCATENATE tl_banco-bankl '-' tl_banco-bkont INTO wa_zfiwrt0021-agencia_banc.
      wa_zfiwrt0021-conta_banc    = tl_banco-bankn.
      SET SCREEN 0.
      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '/00'
        EXCEPTIONS
          function_not_supported = 1.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK
ENDCLASS.

INITIALIZATION.

START-OF-SELECTION.

  wa_zfiwrt0021-fatura_atu = abap_false.

  CALL SCREEN 0100.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.

  IF sy-calld = 'X' AND   wa_zfiwrt0021-contrato IS INITIAL.
    IF vg_chamada NE 'X'.
      GET PARAMETER ID 'CTR' FIELD  wa_zfiwrt0021-contrato.
      GET PARAMETER ID 'BUK' FIELD  wa_zfiwrt0021-bukrs.
      GET PARAMETER ID 'BRC' FIELD  wa_zfiwrt0021-branch.
      GET PARAMETER ID 'KUN' FIELD  wa_zfiwrt0021-kunnr.
      GET PARAMETER ID 'ANO' FIELD  wa_zfiwrt0021-ano.
    ELSE.
      vg_chamada = 'X'.
    ENDIF.
  ENDIF.
  "
  IF  wa_zfiwrt0021-icon = icon_delete.
    APPEND '&EDIT' TO fcode.
    APPEND '&DEL' TO fcode.
  ENDIF.

  IF sy-tcode NE 'ZNFW0008'.
    APPEND '&NEW' TO fcode.
    APPEND '&EDIT' TO fcode.
    APPEND '&DEL' TO fcode.
  ENDIF.

*  AUTHORITY-CHECK OBJECT 'ZNFW008'
*    ID 'ACTVT'  FIELD '01'. "create
*  IF sy-subrc <> 0.
*    APPEND '&NEW' TO fcode.
*  ENDIF.
*
*  AUTHORITY-CHECK OBJECT 'ZNFW008'
*    ID 'ACTVT'  FIELD '02'. "EDIT
*  IF sy-subrc <> 0.
*    APPEND '&EDIT' TO fcode.
*  ENDIF.
*
*  AUTHORITY-CHECK OBJECT 'ZNFW008'
*    ID 'ACTVT'  FIELD '06'. "Eliminar
*  IF sy-subrc <> 0.
*    APPEND '&DEL' TO fcode.
*  ENDIF.

  SET PF-STATUS 'ST_0100'  EXCLUDING fcode.
  SET TITLEBAR 'TL_0100'.

  IF g_custom_container IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'CONTAINER'
        repid          = sy-repid
        dynnr          = sy-dynnr.
  ENDIF.

  IF it_text_edit IS INITIAL.
    CREATE OBJECT it_text_edit
      EXPORTING
        max_number_chars = 255
        parent           = g_custom_container.
  ENDIF.

  IF it_text IS NOT INITIAL AND it_text_edit IS NOT INITIAL.
    CALL METHOD it_text_edit->set_text_as_stream
      EXPORTING
        text = it_text.
  ENDIF.


  CALL METHOD it_text_edit->set_readonly_mode
    EXPORTING
      readonly_mode = vmode.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: wl_answer(1).

  CASE sy-ucomm.
    WHEN '&CLEAR'.
      CLEAR wa_zfiwrt0021.
      wa_zfiwrt0021-fatura_atu = abap_false.

      REFRESH: it_zfiwrt0022,it_zfiwrt0022_2.
      wa_zfiwrt0021-icon = icon_first_page.
      REFRESH it_text.
      wg_flag = 'X'.
      CLEAR wg_acao.
      vedit = abap_false.
    WHEN 'SALVAR'.
      IF wa_zfiwrt0021-icon = icon_delete.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question  = 'Contrato EXCLUIDO, deseja recuperar?'
          IMPORTING
            answer         = wl_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.

        CASE wl_answer.
          WHEN '2' OR 'A'.
            EXIT.
        ENDCASE.
      ENDIF.
      PERFORM salvar_dados.
    WHEN '&NEW'.
      CLEAR wa_zfiwrt0021.
      wa_zfiwrt0021-fatura_atu = abap_false.

      REFRESH: it_zfiwrt0022,it_zfiwrt0022_2.
      wa_zfiwrt0021-icon = icon_create.
      REFRESH it_text.
      wg_flag = 'X'.
      wg_acao = 'NEW'.
      vedit = abap_false.
    WHEN '&DEL'.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question  = 'Tem certeza que deseja excluir este contrato?'
        IMPORTING
          answer         = wl_answer
        EXCEPTIONS
          text_not_found = 1
          OTHERS         = 2.

      CASE wl_answer.
        WHEN '2' OR 'A'.
          EXIT.
      ENDCASE.

      UPDATE zfiwrt0021  SET loekz = 'X'
              WHERE contrato  EQ wa_zfiwrt0021-contrato
              AND bukrs       EQ wa_zfiwrt0021-bukrs
              AND branch      EQ wa_zfiwrt0021-branch
              AND kunnr       EQ wa_zfiwrt0021-kunnr
              AND ano         EQ wa_zfiwrt0021-ano
              AND loekz       EQ ' '.
      COMMIT WORK.
      CLEAR  wg_acao.
      wa_zfiwrt0021-icon = icon_delete.
    WHEN '&EDIT'.
      IF vedit = 'X'.
        CLEAR vedit .
        CLEAR wg_acao.
      ELSE.
        vedit = 'X'.
        wg_acao  = 'UPD'.
      ENDIF.

    WHEN 'BACK' OR '&SAIR'.
      LEAVE TO SCREEN 0.
    WHEN 'SEARCH'.
      PERFORM buscar_dados.
  ENDCASE.

ENDMODULE.

FORM get_next_number USING p_object
                           p_nr_range
                      CHANGING p_number.

  CLEAR p_number.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = p_nr_range
      object                  = p_object
    IMPORTING
      number                  = p_number
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    CLEAR p_number.
    MESSAGE e836(sd) WITH 'O intervalo de numeração não foi encontrado!'.
  ELSE.
    wg_flag = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_CONTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_contrato INPUT.
  PERFORM f_contrato.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_TIPO_CONTRATO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_tipo_contrato INPUT.

  DATA: BEGIN OF tl_tipo_cont OCCURS 0,
          tipo      TYPE zfiwrt0021-tipo,
          descricao TYPE zfit0036-observacao,
        END OF tl_tipo_cont.

  REFRESH tl_tipo_cont.

  tl_tipo_cont-tipo   = 'LP'.
  tl_tipo_cont-descricao   = 'Longo Prazo'.
  APPEND tl_tipo_cont.

  tl_tipo_cont-tipo   = 'CP'.
  tl_tipo_cont-descricao   = 'Curto Prazo'.
  APPEND tl_tipo_cont.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'TIPO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WA_ZFIWRT0021-TIPO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_tipo_cont
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_BRANCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_branch INPUT.

  DATA: BEGIN OF tl_branch OCCURS 0,
          branch TYPE j_1bbranch-branch,
          bukrs  TYPE j_1bbranch-bukrs,
          name   TYPE j_1bbranch-name,
        END OF tl_branch.

  SELECT  branch  bukrs name  FROM j_1bbranch
    INTO TABLE tl_branch.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BRANCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WA_ZFIWRT0021-BRANCH'
      value_org       = 'S'
    TABLES
      value_tab       = tl_branch
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  BUSCA_OPERACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_operacao INPUT.

  DATA: BEGIN OF tl_operacao OCCURS 0,
          operacao  TYPE zfiwrt0001-operacao,
          descricao TYPE zfiwrt0001-descricao,
        END OF tl_operacao.

  REFRESH: tl_operacao.

  SELECT operacao descricao  FROM zfiwrt0001
      INTO TABLE tl_operacao
     WHERE opr_blq EQ 'L'.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'OPERACAO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'WA_ZFIWRT0021-OPERACAO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_operacao
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  BUSCA_FORMA_TIPO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_forma_tipo INPUT.


  CASE wa_zfiwrt0021-tipo.
    WHEN 'LP'.
      wa_zfiwrt0021-desc_tipo = 'Contrato Longo Prazo'.
      vcont  = abap_false.
    WHEN 'CP'.
      wa_zfiwrt0021-desc_tipo = 'Contrato Curto Prazo'.

      IF wa_zfiwrt0021-contrato IS INITIAL.

        PERFORM get_next_number IN PROGRAM zwrr0007 USING  'ZSEQ_CONT'
                                                           '01' CHANGING wa_zfiwrt0021-contrato.
      ENDIF.

      vcont =  abap_true.
    WHEN OTHERS.
      CLEAR wa_zfiwrt0021-desc_tipo.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  BUSCA_FORMA_BUKRS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_forma_bukrs INPUT.

  IF wa_zfiwrt0021-bukrs IS NOT INITIAL.

    CLEAR wa_zfiwrt0021-desc_empresa.

    SELECT SINGLE butxt FROM t001
        INTO wa_zfiwrt0021-desc_empresa
     WHERE bukrs EQ wa_zfiwrt0021-bukrs.

  ELSE.
    CLEAR wa_zfiwrt0021-desc_empresa.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_FORMA_BRANCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_forma_branch INPUT.

  IF wa_zfiwrt0021-branch IS NOT INITIAL.
    CLEAR wa_zfiwrt0021-desc_local.

    SELECT SINGLE name FROM j_1bbranch
        INTO wa_zfiwrt0021-desc_local
     WHERE branch EQ  wa_zfiwrt0021-branch .
  ELSE.
    CLEAR wa_zfiwrt0021-desc_local.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_FORMA_KUNNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_forma_kunnr INPUT.

  IF wa_zfiwrt0021-kunnr IS NOT INITIAL.
    CLEAR wa_zfiwrt0021-desc_cliente.

    SELECT SINGLE name1 FROM kna1
      INTO wa_zfiwrt0021-desc_cliente
     WHERE kunnr EQ wa_zfiwrt0021-kunnr.
  ELSE.
    CLEAR wa_zfiwrt0021-desc_cliente.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_FORMA_MATNR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_forma_matnr INPUT.

  IF wa_zfiwrt0021-matnr IS NOT INITIAL.
    CLEAR wa_zfiwrt0021-desc_material.

    SELECT SINGLE maktx FROM makt
      INTO wa_zfiwrt0021-desc_material
      WHERE matnr EQ wa_zfiwrt0021-matnr
      AND   spras EQ sy-langu.
  ELSE.
    CLEAR wa_zfiwrt0021-desc_material.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  BUSCA_FORMA_OPERACAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_forma_operacao INPUT.

  IF wa_zfiwrt0021-operacao IS NOT INITIAL.
    CLEAR wa_zfiwrt0021-desc_operacao.

    SELECT  SINGLE descricao FROM zfiwrt0001
      INTO wa_zfiwrt0021-desc_operacao
      WHERE operacao EQ wa_zfiwrt0021-operacao.
  ELSE.
    CLEAR wa_zfiwrt0021-desc_operacao.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TRATAR_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tratar_field OUTPUT.
  IF  wa_zfiwrt0021-icon IS INITIAL.
    wa_zfiwrt0021-icon = icon_message_warning.
  ENDIF.
  LOOP AT SCREEN.
    vmode = 1.

    IF  wa_zfiwrt0021-contrato IS NOT INITIAL AND
        wa_zfiwrt0021-tipo  IS NOT INITIAL AND
        wa_zfiwrt0021-bukrs IS NOT INITIAL AND
        wa_zfiwrt0021-branch IS NOT INITIAL AND
        wa_zfiwrt0021-kunnr IS NOT INITIAL AND
        wa_zfiwrt0021-ano IS NOT INITIAL.
      IF  screen-name <> 'WA_ZFIWRT0021-CONTRATO'  AND
          screen-name <> 'WA_ZFIWRT0021-TIPO'    AND
          screen-name <> 'WA_ZFIWRT0021-BUKRS'   AND
          screen-name <> 'WA_ZFIWRT0021-BRANCH'  AND
          screen-name <> 'WA_ZFIWRT0021-KUNNR'   AND
          screen-name <> 'WA_ZFIWRT0021-ANO' .
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        vmode = 0.
        IF wg_acao = 'UPD'.
          vedit = 'X'.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR vedit.
    ENDIF.


    IF vcont IS NOT INITIAL.
      IF screen-name = 'WA_ZFIWRT0021-CONTRATO'.
        screen-input     = 0.
        screen-active    = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

    IF vedit IS NOT INITIAL.
      IF ( screen-name = 'WA_ZFIWRT0021-CONTRATO' OR  screen-name = 'WA_ZFIWRT0021-BUKRS'   OR
           screen-name = 'WA_ZFIWRT0021-BRANCH'   OR  screen-name = 'WA_ZFIWRT0021-TOTAL' OR
           screen-name = 'WA_ZFIWRT0021-TIPO' OR
           screen-name = 'WA_ZFIWRT0021-KUNNR'   OR  screen-name = 'WA_ZFIWRT0021-ANO' ).
        screen-input     = 0.
        screen-active    = 1.
        MODIFY SCREEN.
        vmode = 1.
      ELSE.
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        vmode = 0.
      ENDIF.
    ELSE.
      IF ( screen-name = 'WA_ZFIWRT0021-CONTRATO' OR  screen-name = 'WA_ZFIWRT0021-BUKRS'   OR
        screen-name = 'WA_ZFIWRT0021-BRANCH'  OR  screen-name = 'WA_ZFIWRT0021-TIPO' OR
        screen-name = 'WA_ZFIWRT0021-KUNNR'   OR  screen-name = 'WA_ZFIWRT0021-ANO' ).
        screen-input     = 1.
        screen-active    = 1.
        MODIFY SCREEN.
        vmode = 1.
      ELSE.
        screen-input     = 0.
        screen-active    = 1.
        MODIFY SCREEN.
        vmode = 0.
      ENDIF.
    ENDIF.

    CASE screen-name.
      WHEN 'WA_ZFIWRT0021-TARIFA01' OR 'WA_ZFIWRT0021-MONTANTE01'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '01' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '01' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.

            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA02' OR 'WA_ZFIWRT0021-MONTANTE02'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '02' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '02' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA03' OR 'WA_ZFIWRT0021-MONTANTE03'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '03' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '03' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.

            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA04' OR 'WA_ZFIWRT0021-MONTANTE04'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '04' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '04' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA05' OR 'WA_ZFIWRT0021-MONTANTE05'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '05' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '05' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA06' OR 'WA_ZFIWRT0021-MONTANTE06'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '06' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '06' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA07' OR 'WA_ZFIWRT0021-MONTANTE07'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '07' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '07' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA08' OR 'WA_ZFIWRT0021-MONTANTE08'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '08' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '08' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA09' OR 'WA_ZFIWRT0021-MONTANTE09'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '09' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '09' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA10' OR 'WA_ZFIWRT0021-MONTANTE10'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '10' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '10' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA11' OR 'WA_ZFIWRT0021-MONTANTE11'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '11' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '11' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        "
      WHEN 'WA_ZFIWRT0021-TARIFA12' OR 'WA_ZFIWRT0021-MONTANTE12'.
        READ TABLE it_zfiwrt0022 INTO wa_zfiwrt0022 WITH KEY mes = '12' BINARY SEARCH.
        IF  sy-subrc = 0.
          IF wa_zfiwrt0022-data_fatura IS NOT INITIAL.
            screen-input     = 0.
            screen-active    = 1.
            MODIFY SCREEN.
          ENDIF.
        ELSE. "Checa se contabil está estornado
          READ TABLE it_zfiwrt0022_2 INTO wa_zfiwrt0022_2 WITH KEY mes = '12' BINARY SEARCH.
          IF  sy-subrc = 0.
            CLEAR tl_docest.
            READ TABLE tl_docest
               WITH KEY seq_lcto = wa_zfiwrt0022_2-seq_lcto
                   BINARY SEARCH.
            IF tl_docest-belnr_est IS INITIAL.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
        " WHEN 'WA_ZFIWRT0021-FATURA_F' OR 'WA_ZFIWRT0021-FATURA_U'.
      WHEN  'WA_ZFIWRT0021-FATURA_U'.
        IF sy-ucomm = 'GER_AUT' OR wa_zfiwrt0021-ger_autom = 'X'.
*          screen-input     = 1.
*          screen-active    = 1.
*          MODIFY SCREEN.
          IF wa_zfiwrt0021-fatura_f IS NOT INITIAL.
            CLEAR wa_zfiwrt0021-fatura_u.
            IF screen-name = 'WA_ZFIWRT0021-FATURA_U'.
              screen-input     = 0.
              screen-active    = 1.
              MODIFY SCREEN.
            ENDIF.
          ELSEIF wa_zfiwrt0021-fatura_u IS NOT INITIAL.
            IF screen-name = 'WA_ZFIWRT0021-FATURA_F'.
*              screen-input     = 0.
*              screen-active    = 1.
*              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ELSE.
          screen-input     = 0.
          screen-active    = 1.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  BUSCAR_DAOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buscar_dados.
  DATA wa_zfiwrt0021_cop TYPE ty_zfiwrt0021.
  DATA w_contrato        TYPE zfiwrt0021-contrato.
  DATA w_contratoc(10).
  DATA: g_type TYPE dd01v-datatype.

  IF vedit IS INITIAL.
    REFRESH it_text.
    CLEAR wa_text.

    REFRESH it_t0021.
    IF wa_zfiwrt0021-contrato IS NOT INITIAL AND
       wa_zfiwrt0021-bukrs    IS INITIAL AND
       wa_zfiwrt0021-branch   IS INITIAL AND
       wa_zfiwrt0021-kunnr    IS INITIAL AND
       wa_zfiwrt0021-ano      IS INITIAL.
      "
      w_contrato =  wa_zfiwrt0021-contrato.
      "
      SELECT * FROM  zfiwrt0021  INTO TABLE it_t0021
           WHERE contrato  EQ w_contrato.
      IF sy-subrc NE 0.
        w_contratoc =  w_contrato.
        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            string_in = w_contratoc
          IMPORTING
            htype     = g_type.
        "
        IF g_type = 'NUMC'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = w_contratoc
            IMPORTING
              output = w_contratoc.
          w_contrato = w_contratoc.
          SELECT * FROM  zfiwrt0021  INTO TABLE it_t0021
              WHERE contrato  EQ w_contrato.
        ENDIF.
      ENDIF.

      IF lines( it_t0021 ) GT 1.
        PERFORM f_contrato.
      ELSEIF lines( it_t0021 ) EQ 1.
        READ TABLE it_t0021 INTO wa_t0021 INDEX 1.
        wa_zfiwrt0021-contrato = wa_t0021-contrato.
        wa_zfiwrt0021-bukrs    = wa_t0021-bukrs.
        wa_zfiwrt0021-branch   = wa_t0021-branch.
        wa_zfiwrt0021-kunnr    = wa_t0021-kunnr.
        wa_zfiwrt0021-ano      = wa_t0021-ano.
      ENDIF.
    ENDIF.

    SELECT * FROM  zfiwrt0021  INTO TABLE it_t0021
     WHERE contrato  EQ wa_zfiwrt0021-contrato
     AND bukrs       EQ wa_zfiwrt0021-bukrs
     AND branch      EQ wa_zfiwrt0021-branch
     AND kunnr       EQ wa_zfiwrt0021-kunnr
     AND ano         EQ wa_zfiwrt0021-ano.



    READ TABLE it_t0021 INTO wa_t0021 INDEX 1.
    IF sy-subrc = 0.
      wa_zfiwrt0021-contrato = wa_t0021-contrato.
      wa_zfiwrt0021-bukrs    = wa_t0021-bukrs.
      wa_zfiwrt0021-branch   = wa_t0021-branch.
      wa_zfiwrt0021-kunnr    = wa_t0021-kunnr.
      wa_zfiwrt0021-ano      = wa_t0021-ano.
      IF wa_t0021-loekz = 'X'.
        wa_zfiwrt0021-icon = icon_delete.
      ELSE.
        wa_zfiwrt0021-icon = icon_checked.
      ENDIF.
      wa_zfiwrt0021-bukrs       =   wa_t0021-bukrs.
      SELECT SINGLE butxt FROM t001    INTO wa_zfiwrt0021-desc_empresa
       WHERE bukrs EQ wa_t0021-bukrs.

      wa_zfiwrt0021-branch      = wa_t0021-branch.
      SELECT SINGLE name FROM j_1bbranch INTO wa_zfiwrt0021-desc_local
        WHERE branch EQ  wa_t0021-branch.

      wa_zfiwrt0021-data_inicio     = wa_t0021-data_inicio.
      wa_zfiwrt0021-data_final      = wa_t0021-data_final.
      wa_zfiwrt0021-kunnr           = wa_t0021-kunnr.
      SELECT SINGLE name1 FROM kna1 INTO wa_zfiwrt0021-desc_cliente
        WHERE kunnr EQ wa_t0021-kunnr.


      wa_zfiwrt0021-banco        = wa_t0021-banco.
      wa_zfiwrt0021-agencia_banc = wa_t0021-agencia_banc .
      wa_zfiwrt0021-conta_banc   = wa_t0021-conta_banc .

      wa_zfiwrt0021-vencimento_f = wa_t0021-vencimento_f.
      wa_zfiwrt0021-vencimento   = wa_t0021-vencimento.
      wa_zfiwrt0021-texto_nota   = wa_t0021-texto_nota.
      wa_zfiwrt0021-operacao     = wa_t0021-operacao.
      SELECT SINGLE descricao FROM zfiwrt0001  INTO wa_zfiwrt0021-desc_operacao
        WHERE operacao EQ wa_t0021-operacao.

      wa_zfiwrt0021-matnr       = wa_t0021-matnr.
      SELECT SINGLE maktx FROM makt INTO wa_zfiwrt0021-desc_material
        WHERE matnr EQ wa_t0021-matnr.

      wa_zfiwrt0021-ger_autom   = wa_t0021-ger_autom.

      wa_zfiwrt0021-pgto_mes_seguinte   = wa_t0021-pgto_mes_seguinte.
      wa_zfiwrt0021-fatura_atu   = wa_t0021-fatura_atu.
      wa_zfiwrt0021-fatura_f     = wa_t0021-fatura_f.
      wa_zfiwrt0021-fatura_u     = wa_t0021-fatura_u.

      wa_zfiwrt0021-tipo        = wa_t0021-tipo.
      IF wa_t0021-tipo = 'L'.
        wa_zfiwrt0021-tipo = 'LP'.
      ELSEIF   wa_t0021-tipo = 'C'.
        wa_zfiwrt0021-tipo  = 'CP'.
      ENDIF.

      IF wa_t0021-tipo = 'LP'.
        wa_zfiwrt0021-desc_tipo = 'Contrato Longo Prazo'.
      ELSEIF   wa_t0021-tipo = 'CP'.
        wa_zfiwrt0021-desc_tipo = 'Contrato Curto Prazo'.
      ENDIF.

      wa_zfiwrt0021-montante01   =  wa_t0021-montante01.
      wa_zfiwrt0021-tarifa01     =  wa_t0021-tarifa01.
      wa_zfiwrt0021-montante02   =  wa_t0021-montante02.
      wa_zfiwrt0021-tarifa02     =  wa_t0021-tarifa02.
      wa_zfiwrt0021-montante03   =  wa_t0021-montante03.
      wa_zfiwrt0021-tarifa03     =  wa_t0021-tarifa03.
      wa_zfiwrt0021-montante04   =  wa_t0021-montante04.
      wa_zfiwrt0021-tarifa04     =  wa_t0021-tarifa04.
      wa_zfiwrt0021-montante05   =  wa_t0021-montante05.
      wa_zfiwrt0021-tarifa05     =  wa_t0021-tarifa05.
      wa_zfiwrt0021-montante06   =  wa_t0021-montante06.
      wa_zfiwrt0021-tarifa06     =  wa_t0021-tarifa06.
      wa_zfiwrt0021-montante07   =  wa_t0021-montante07.
      wa_zfiwrt0021-tarifa07     =  wa_t0021-tarifa07.
      wa_zfiwrt0021-montante08   =  wa_t0021-montante08.
      wa_zfiwrt0021-tarifa08     =  wa_t0021-tarifa08.
      wa_zfiwrt0021-montante09   =  wa_t0021-montante09.
      wa_zfiwrt0021-tarifa09     =  wa_t0021-tarifa09.
      wa_zfiwrt0021-montante10   =  wa_t0021-montante10.
      wa_zfiwrt0021-tarifa10     =  wa_t0021-tarifa10.
      wa_zfiwrt0021-montante11   =  wa_t0021-montante11.
      wa_zfiwrt0021-tarifa11     =  wa_t0021-tarifa11.
      wa_zfiwrt0021-montante12   =  wa_t0021-montante12.
      wa_zfiwrt0021-tarifa12     =  wa_t0021-tarifa12.

      wa_text = wa_t0021-texto_nota.
      APPEND wa_text TO it_text.

      REFRESH: it_zfiwrt0022,it_zfiwrt0022_2.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_zfiwrt0022
        FROM zfiwrt0022
        INNER JOIN zfiwrt0008
          ON  zfiwrt0008~seq_lcto        EQ zfiwrt0022~seq_lcto
          AND zfiwrt0008~docs_estornados EQ ' '
        WHERE zfiwrt0022~contrato  EQ wa_zfiwrt0021-contrato
        AND   zfiwrt0022~bukrs     EQ wa_zfiwrt0021-bukrs
        AND   zfiwrt0022~branch    EQ wa_zfiwrt0021-branch
        AND   zfiwrt0022~kunnr     EQ wa_zfiwrt0021-kunnr
        AND   zfiwrt0022~ano       EQ wa_zfiwrt0021-ano.

      "documento contábil
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_zfiwrt0022_2
        FROM zfiwrt0022
        INNER JOIN zfiwrt0008
          ON  zfiwrt0008~seq_lcto        EQ zfiwrt0022~seq_lcto
          AND zfiwrt0008~docs_estornados EQ 'X'
        WHERE zfiwrt0022~contrato  EQ wa_zfiwrt0021-contrato
        AND   zfiwrt0022~bukrs     EQ wa_zfiwrt0021-bukrs
        AND   zfiwrt0022~branch    EQ wa_zfiwrt0021-branch
        AND   zfiwrt0022~kunnr     EQ wa_zfiwrt0021-kunnr
        AND   zfiwrt0022~ano       EQ wa_zfiwrt0021-ano.

      REFRESH tl_docest.
      LOOP AT it_zfiwrt0022_2 INTO wa_zfiwrt0022_2.
        MOVE: wa_zfiwrt0022_2-seq_lcto TO tl_docest-seq_lcto.

        APPEND tl_docest.
        CLEAR: tl_docest.
      ENDLOOP.

      CALL FUNCTION 'ZNFW_ESTORNA_SEQ_LCTO'
        TABLES
          t_docs = tl_docest.

      SORT  tl_docest BY seq_lcto.

      SORT  it_zfiwrt0022 BY mes.
      SORT  it_zfiwrt0022_2 BY mes.
    ELSE.
      REFRESH: it_zfiwrt0022,it_zfiwrt0022_2.
      MOVE-CORRESPONDING wa_zfiwrt0021 TO wa_zfiwrt0021_cop.
      CLEAR wa_zfiwrt0021.
      wa_zfiwrt0021-fatura_atu    = wa_zfiwrt0021_cop-fatura_atu.
      wa_zfiwrt0021-tipo          = wa_zfiwrt0021_cop-tipo.
      wa_zfiwrt0021-desc_tipo     = wa_zfiwrt0021_cop-desc_tipo.
      wa_zfiwrt0021-contrato      = wa_zfiwrt0021_cop-contrato.
      wa_zfiwrt0021-bukrs         = wa_zfiwrt0021_cop-bukrs.
      wa_zfiwrt0021-desc_empresa  = wa_zfiwrt0021_cop-desc_empresa.
      wa_zfiwrt0021-branch        = wa_zfiwrt0021_cop-branch.
      wa_zfiwrt0021-desc_local    = wa_zfiwrt0021_cop-desc_local.
      wa_zfiwrt0021-kunnr         = wa_zfiwrt0021_cop-kunnr.
      wa_zfiwrt0021-desc_cliente  = wa_zfiwrt0021_cop-desc_cliente.
      wa_zfiwrt0021-ano           = wa_zfiwrt0021_cop-ano.
      IF  wg_acao EQ 'NEW'.
        vedit   = abap_true.
      ELSE.
        vedit   = abap_false.
      ENDIF.
    ENDIF.

    wa_zfiwrt0021-total = ( wa_zfiwrt0021-tarifa01 * wa_zfiwrt0021-montante01 ) +
                          ( wa_zfiwrt0021-tarifa02 * wa_zfiwrt0021-montante02 ) +
                          ( wa_zfiwrt0021-tarifa03 * wa_zfiwrt0021-montante03 ) +
                          ( wa_zfiwrt0021-tarifa04 * wa_zfiwrt0021-montante04 ) +
                          ( wa_zfiwrt0021-tarifa05 * wa_zfiwrt0021-montante05 ) +
                          ( wa_zfiwrt0021-tarifa06 * wa_zfiwrt0021-montante06 ) +
                          ( wa_zfiwrt0021-tarifa07 * wa_zfiwrt0021-montante07 ) +
                          ( wa_zfiwrt0021-tarifa08 * wa_zfiwrt0021-montante08 ) +
                          ( wa_zfiwrt0021-tarifa09 * wa_zfiwrt0021-montante09 ) +
                          ( wa_zfiwrt0021-tarifa10 * wa_zfiwrt0021-montante10 ) +
                          ( wa_zfiwrt0021-tarifa11 * wa_zfiwrt0021-montante11 ) +
                          ( wa_zfiwrt0021-tarifa12 * wa_zfiwrt0021-montante12 ) .

    wa_zfiwrt0021-total_montante = wa_zfiwrt0021-montante01 +
                                   wa_zfiwrt0021-montante02 +
                                   wa_zfiwrt0021-montante03 +
                                   wa_zfiwrt0021-montante04 +
                                   wa_zfiwrt0021-montante05 +
                                   wa_zfiwrt0021-montante06 +
                                   wa_zfiwrt0021-montante07 +
                                   wa_zfiwrt0021-montante08 +
                                   wa_zfiwrt0021-montante09 +
                                   wa_zfiwrt0021-montante10 +
                                   wa_zfiwrt0021-montante11 +
                                   wa_zfiwrt0021-montante12 .

    IF wa_text IS NOT INITIAL.
      APPEND wa_text TO it_text.
    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SALVAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM salvar_dados.

  IF wa_zfiwrt0021-contrato IS INITIAL.
    MESSAGE 'Informe o contrato' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-bukrs IS INITIAL.
    MESSAGE 'Informe a empresa' TYPE 'I'.
    EXIT.
  ELSE.
    SELECT SINGLE *
     FROM t001
     INTO @DATA(_t001)
     WHERE bukrs = @wa_zfiwrt0021-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Empresa não existe' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
  IF wa_zfiwrt0021-branch IS INITIAL.
    MESSAGE 'Informe o local de negócios' TYPE 'I'.
    EXIT.
  ELSE.
    SELECT SINGLE *
    FROM j_1bbranch
    INTO @DATA(_j_1bbranch)
    WHERE branch = @wa_zfiwrt0021-branch
    AND   bukrs  = @wa_zfiwrt0021-bukrs.
    IF sy-subrc NE 0.
      MESSAGE 'Local de Negócios não existe' TYPE 'I'.
      EXIT.
    ENDIF.

  ENDIF.
  IF wa_zfiwrt0021-ano IS INITIAL.
    MESSAGE 'Informe o ano do contrato' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-data_inicio IS INITIAL.
    MESSAGE 'Informe a data início' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-data_final IS INITIAL.
    MESSAGE 'Informe a data final' TYPE 'I'.
    EXIT.
  ELSEIF  wa_zfiwrt0021-data_final LT  wa_zfiwrt0021-data_inicio.
    MESSAGE 'Data final menor que data Inicial' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-kunnr IS INITIAL.
    MESSAGE 'Informe o cliente' TYPE 'I'.
    EXIT.
  ELSE.
    SELECT SINGLE *
      FROM kna1
      INTO @DATA(_kna1)
      WHERE kunnr = @wa_zfiwrt0021-kunnr.
    IF sy-subrc NE 0.
      MESSAGE 'Cliente não existe' TYPE 'I'.
      EXIT.
    ELSE.
      SELECT SINGLE *
      FROM knb1
      INTO @DATA(_knb1)
      WHERE kunnr = @wa_zfiwrt0021-kunnr
      AND   bukrs = @wa_zfiwrt0021-bukrs.
      IF sy-subrc NE 0.
        MESSAGE 'Cliente não existe na empresa' TYPE 'I'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.
  IF wa_zfiwrt0021-vencimento_f IS INITIAL AND wa_zfiwrt0021-vencimento IS INITIAL.
    MESSAGE 'Informe um vencimento' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-banco IS INITIAL.
    MESSAGE 'Informe o banco' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-agencia_banc IS INITIAL.
    MESSAGE 'Informe a agencia bancária' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-conta_banc IS INITIAL.
    MESSAGE 'Informe a conta bancária' TYPE 'I'.
    EXIT.
  ENDIF.
  IF wa_zfiwrt0021-operacao IS INITIAL.
    MESSAGE 'Informe a operação' TYPE 'I'.
    EXIT.
  ELSE.
    SELECT SINGLE *
     FROM zfiwrt0001
     INTO @DATA(_zfiwrt0001)
     WHERE operacao = @wa_zfiwrt0021-operacao.
    IF sy-subrc NE 0.
      MESSAGE 'Operação não existe' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
  IF wa_zfiwrt0021-matnr IS INITIAL.
    MESSAGE 'Informe o material' TYPE 'I'.
    EXIT.
  ELSE.
    SELECT SINGLE *
     FROM marc
     INTO @DATA(_marc)
     WHERE matnr = @wa_zfiwrt0021-matnr
     AND   werks = @wa_zfiwrt0021-branch.
    IF sy-subrc NE 0.
      MESSAGE 'Material não existe no centro' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
  IF wa_zfiwrt0021-tipo IS INITIAL.
    MESSAGE 'Informe o tipo do contrato' TYPE 'I'.
    EXIT.
  ELSEIF wa_zfiwrt0021-tipo NE 'LP' AND wa_zfiwrt0021-tipo NE 'CP'.
    MESSAGE 'Tipo do contrato inválido' TYPE 'I'.
    EXIT.
  ENDIF.

  IF wa_zfiwrt0021-ger_autom  IS NOT INITIAL.
    IF wa_zfiwrt0021-fatura_f IS INITIAL AND wa_zfiwrt0021-fatura_u IS INITIAL.
      MESSAGE 'Dia Fixo ou Util Faturamento devem ser informado!' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.
*if WA_ZFIWRT0021-MONTANTE01
*if WA_ZFIWRT0021-TARIFA01

  CALL METHOD it_text_edit->get_text_as_stream
    IMPORTING
      text = it_text.

  READ TABLE it_text INTO wa_text INDEX 1.
  wa_zfiwrt0021-texto_nota = wa_text.

  wa_zfiwrt0021-usuario = sy-uname.
  wa_zfiwrt0021-dt_modf = sy-datum.
  wa_zfiwrt0021-hr_modf = sy-uzeit.

  MOVE-CORRESPONDING wa_zfiwrt0021 TO wa_saida.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = wa_saida-matnr
    IMPORTING
      output = wa_saida-matnr.
  APPEND wa_saida TO it_saida.

  MODIFY zfiwrt0021 FROM TABLE it_saida.

  CLEAR: wa_zfiwrt0021, wa_saida.
  wa_zfiwrt0021-fatura_atu = abap_false.

  REFRESH:  it_saida, it_text.

  IF it_saida[] IS INITIAL.
    MESSAGE 'Dados gravado com sucesso!' TYPE 'I'.
  ENDIF.

  CALL METHOD it_text_edit->set_text_as_stream
    EXPORTING
      text = it_text.

  CLEAR: wg_acao, vedit.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'ST_0500'.
  SET TITLEBAR 'TL_0500'.

  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function  WITH HEADER LINE.

  IF obg_conteiner_obj IS INITIAL.
    CREATE OBJECT obg_conteiner_obj
      EXPORTING
        container_name = 'CC_CTR'.


    CREATE OBJECT grid5
      EXPORTING
        i_parent = obg_conteiner_obj.

    PERFORM montar_layout.
    CLEAR wa_layout.
    wa_layout-no_toolbar = ' '.
    gs_variant_c-report = sy-repid.
    CALL METHOD grid5->set_table_for_first_display
      EXPORTING
        is_variant      = gs_variant_c
        is_layout       = wa_layout
      CHANGING
        it_filter       = tl_filter
        it_fieldcatalog = tg_fieldcatalog[]
        it_outtab       = tl_zfiw0021[].

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid5->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:  lcl_event_handler=>on_double_click          FOR grid5.

  ELSE.
    CALL METHOD grid5->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  REFRESH tg_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ' '       ' '                 'TL_ZFIW0021'  'TIPO'                 'Tipo'              '06'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_ZFIW0021'  'CONTRATO'             'Contrato'          '12'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_ZFIW0021'  'BUKRS'                'Empresa'           '10'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_ZFIW0021'  'BRANCH'               'Local'             '10'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_ZFIW0021'  'KUNNR'                'Cliente'           '10'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_ZFIW0021'  'NAME1'                'Nome'              '20'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_ZFIW0021'  'ANO'                  'Ano'               '06'  ' ' ' ' ' ' ' ' ' '.

ENDFORM.

FORM f_montar_estrutura  USING  p_col_pos     p_ref_tabname   p_ref_fieldname
                                p_tabname     p_field         p_scrtext_l
                                p_outputlen   p_edit          p_sum
                                p_emphasize   p_f4            p_ico.

  CLEAR wg_fieldcatalog.
  wg_fieldcatalog-fieldname    = p_field.
  wg_fieldcatalog-tabname      = p_tabname.
  wg_fieldcatalog-ref_table    = p_ref_tabname.
  wg_fieldcatalog-ref_field    = p_ref_fieldname.
  wg_fieldcatalog-key          = ' '.

  wg_fieldcatalog-edit         = p_edit.
  wg_fieldcatalog-do_sum       = p_sum.

  wg_fieldcatalog-col_pos      = p_col_pos.

  IF p_outputlen IS NOT INITIAL.
    wg_fieldcatalog-outputlen  = p_outputlen.
  ENDIF.

  wg_fieldcatalog-no_out       = ' '.
  wg_fieldcatalog-reptext      = p_scrtext_l.
  wg_fieldcatalog-scrtext_s    = p_scrtext_l.
  wg_fieldcatalog-scrtext_m    = p_scrtext_l.
  wg_fieldcatalog-scrtext_l    = p_scrtext_l.
  wg_fieldcatalog-emphasize    = p_emphasize.

  IF p_f4 IS NOT INITIAL.
    wg_fieldcatalog-f4availabl = 'X'.
  ENDIF.


  IF p_ico IS NOT INITIAL.
    wg_fieldcatalog-hotspot    = 'X'.
    wg_fieldcatalog-icon       = 'X'.
  ENDIF.

  APPEND wg_fieldcatalog TO tg_fieldcatalog.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.
  DATA: tg_selectedrow TYPE lvc_t_row,
        wg_selectedrow TYPE lvc_s_row.
  CASE ok-code.
    WHEN '&SEL'.
      CALL METHOD grid5->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF lines( tg_selectedrow[] ) NE 1.
        MESSAGE 'Selecione  uma linha!' TYPE 'E'.
        EXIT.
      ENDIF.
      LOOP AT tg_selectedrow INTO wg_selectedrow.
        READ TABLE tl_zfiw0021 INDEX wg_selectedrow-index.
        wa_zfiwrt0021-contrato = tl_zfiw0021-contrato.
        wa_zfiwrt0021-bukrs    = tl_zfiw0021-bukrs.
        wa_zfiwrt0021-branch   = tl_zfiw0021-branch.
        wa_zfiwrt0021-kunnr    = tl_zfiw0021-kunnr.
        wa_zfiwrt0021-ano      = tl_zfiw0021-ano.
      ENDLOOP.
*      SET SCREEN 0.
      LEAVE TO SCREEN 0.
    WHEN '&SAIR'.
*      IF grid5 IS NOT INITIAL.
*        CALL METHOD grid5->free.
*
*        IF obg_conteiner_obj IS NOT INITIAL.
*          CALL METHOD obg_conteiner_obj->free.
*        ENDIF.
*        FREE: obg_conteiner_obj, grid5.
*      ENDIF.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_TOTAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_total INPUT.
  wa_zfiwrt0021-total = ( wa_zfiwrt0021-tarifa01 * wa_zfiwrt0021-montante01 ) +
                        ( wa_zfiwrt0021-tarifa02 * wa_zfiwrt0021-montante02 ) +
                        ( wa_zfiwrt0021-tarifa03 * wa_zfiwrt0021-montante03 ) +
                        ( wa_zfiwrt0021-tarifa04 * wa_zfiwrt0021-montante04 ) +
                        ( wa_zfiwrt0021-tarifa05 * wa_zfiwrt0021-montante05 ) +
                        ( wa_zfiwrt0021-tarifa06 * wa_zfiwrt0021-montante06 ) +
                        ( wa_zfiwrt0021-tarifa07 * wa_zfiwrt0021-montante07 ) +
                        ( wa_zfiwrt0021-tarifa08 * wa_zfiwrt0021-montante08 ) +
                        ( wa_zfiwrt0021-tarifa09 * wa_zfiwrt0021-montante09 ) +
                        ( wa_zfiwrt0021-tarifa10 * wa_zfiwrt0021-montante10 ) +
                        ( wa_zfiwrt0021-tarifa11 * wa_zfiwrt0021-montante11 ) +
                        ( wa_zfiwrt0021-tarifa12 * wa_zfiwrt0021-montante12 ) .

  wa_zfiwrt0021-total_montante = wa_zfiwrt0021-montante01 +
                                 wa_zfiwrt0021-montante02 +
                                 wa_zfiwrt0021-montante03 +
                                 wa_zfiwrt0021-montante04 +
                                 wa_zfiwrt0021-montante05 +
                                 wa_zfiwrt0021-montante06 +
                                 wa_zfiwrt0021-montante07 +
                                 wa_zfiwrt0021-montante08 +
                                 wa_zfiwrt0021-montante09 +
                                 wa_zfiwrt0021-montante10 +
                                 wa_zfiwrt0021-montante11 +
                                 wa_zfiwrt0021-montante12 .

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0600 OUTPUT.
  SET PF-STATUS 'ST_0500'.
  SET TITLEBAR 'TL_0600'.

*  DATA: EVENT       TYPE CNTL_SIMPLE_EVENT,
*        EVENTS      TYPE CNTL_SIMPLE_EVENTS,
*        TL_FILTER   TYPE LVC_T_FILT,
*        WL_FILTER   TYPE LVC_S_FILT,
*        TL_FUNCTION TYPE UI_FUNCTIONS,
*        WL_FUNCTION LIKE TL_FUNCTION  WITH HEADER LINE.

  IF obg_conteiner_ban IS INITIAL.
    CREATE OBJECT obg_conteiner_ban
      EXPORTING
        container_name = 'CC_BAN'.


    CREATE OBJECT grid4
      EXPORTING
        i_parent = obg_conteiner_ban.

    PERFORM montar_layout_ban.

    CLEAR wa_layout.
    wa_layout-no_toolbar = ' '.
    gs_variant_c-report = sy-repid.

    CALL METHOD grid4->set_table_for_first_display
      EXPORTING
        is_variant      = gs_variant_c
        is_layout       = wa_layout
      CHANGING
        it_filter       = tl_filter
        it_fieldcatalog = tg_fieldcatalog[]
        it_outtab       = tl_banco[].

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:  lcl_event_handler=>on_double_click4          FOR grid4.

  ELSE.
    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0600 INPUT.
*  DATA: TG_SELECTEDROW TYPE LVC_T_ROW,
*        WG_SELECTEDROW TYPE LVC_S_ROW.
  CASE ok-code.
    WHEN '&SEL'.
      CALL METHOD grid4->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      IF lines( tg_selectedrow[] ) NE 1.
        MESSAGE 'Selecione  uma linha!' TYPE 'E'.
        EXIT.
      ENDIF.
      LOOP AT tg_selectedrow INTO wg_selectedrow.
        READ TABLE tl_banco INDEX wg_selectedrow-index.
        wa_zfiwrt0021-banco         = tl_banco-banco.
*        WA_ZFIWRT0021-AGENCIA_BANC  = TL_BANCO-BANKL.
        CONCATENATE tl_banco-bankl '-' tl_banco-bkont INTO wa_zfiwrt0021-agencia_banc.
        wa_zfiwrt0021-conta_banc    = tl_banco-bankn.
      ENDLOOP.
      SET SCREEN 0.
    WHEN '&SAIR'.
      IF grid4 IS NOT INITIAL.
        CALL METHOD grid4->free.

        IF obg_conteiner_ban IS NOT INITIAL.
          CALL METHOD obg_conteiner_ban->free.
        ENDIF.
        FREE: obg_conteiner_ban, grid4.
      ENDIF.
      SET SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  BUSCA_BANCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE busca_banco INPUT.
  DATA: tl_dynpfields TYPE TABLE OF dynpread,
        wl_dynpfields TYPE dynpread.

  DATA vlifnr  TYPE lfbk-lifnr.

  vlifnr = wa_zfiwrt0021-branch.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = vlifnr
    IMPORTING
      output = vlifnr.

  REFRESH tl_dynpfields.

  SELECT banks bankl bkont bankn
     FROM lfbk INTO TABLE tl_banco
     WHERE lifnr = vlifnr.
  LOOP AT tl_banco.
    IF tl_banco-bankl+0(3) = '001'.
      tl_banco-banco = 'Banco do Brasil'.
    ELSEIF tl_banco-bankl+0(3) = '341'.
      tl_banco-banco = 'Banco Itau'.
    ELSEIF tl_banco-bankl+0(3) = '237'.
      tl_banco-banco = 'Banco Bradesco'.
    ELSEIF tl_banco-bankl+0(3) = '033'.
      tl_banco-banco = 'Banco Santander'.
    ENDIF.
    MODIFY tl_banco INDEX sy-tabix TRANSPORTING banco.
  ENDLOOP.

  CALL SCREEN 0600 STARTING AT 010 1
                    ENDING  AT 90 10.

  MOVE: 'WA_ZFIWRT0021-BANCO'        TO wl_dynpfields-fieldname,
         wa_zfiwrt0021-banco         TO wl_dynpfields-fieldvalue.
  APPEND wl_dynpfields TO tl_dynpfields.
  "
  MOVE: 'WA_ZFIWRT0021-AGENCIA_BANC'      TO wl_dynpfields-fieldname,
         wa_zfiwrt0021-agencia_banc+4(6)     TO wl_dynpfields-fieldvalue.
  APPEND wl_dynpfields TO tl_dynpfields.
  "
  MOVE: 'WA_ZFIWRT0021-CONTA_BANC'        TO wl_dynpfields-fieldname,
        wa_zfiwrt0021-conta_banc          TO wl_dynpfields-fieldvalue.
  APPEND wl_dynpfields TO tl_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = tl_dynpfields.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '/00'
    EXCEPTIONS
      function_not_supported = 1.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_BAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_ban .
  REFRESH tg_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ' '       ' '                 'TL_BANCO'      'BANKL'                 'Agencia'         '15'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_BANCO'      'BKONT'                 'Dig'             '05'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_BANCO'      'BANKN'                 'Conta'           '15'  ' ' ' ' ' ' ' ' ' ',
        1   ' '       ' '                 'TL_BANCO'      'BANCO'                 'Banco'           '30'  ' ' ' ' ' ' ' ' ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CONTRATO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_contrato .
  DATA: tl_dynpfields TYPE TABLE OF dynpread,
        wl_dynpfields TYPE dynpread.

  DATA: r_contrato TYPE RANGE OF zfiwrt0021-contrato,
        w_contrato LIKE LINE OF r_contrato,

        r_bukrs    TYPE RANGE OF zfiwrt0021-bukrs,
        w_bukrs    LIKE LINE OF r_bukrs,

        r_branch   TYPE RANGE OF zfiwrt0021-branch,
        w_branch   LIKE LINE OF r_branch,

        r_kunnr    TYPE RANGE OF zfiwrt0021-kunnr,
        w_kunnr    LIKE LINE OF r_kunnr,

        r_ano      TYPE RANGE OF zfiwrt0021-ano,
        w_ano      LIKE LINE OF r_ano.


  REFRESH: tl_dynpfields,
            r_contrato,
            r_bukrs,
            r_branch,
            r_kunnr,
            r_ano.

  MOVE: 'WA_ZFIWRT0021-CONTRATO'        TO wl_dynpfields-fieldname.
  APPEND wl_dynpfields TO tl_dynpfields.

  MOVE: 'WA_ZFIWRT0021-BUKRS'        TO wl_dynpfields-fieldname.
  APPEND wl_dynpfields TO tl_dynpfields.

  MOVE: 'WA_ZFIWRT0021-BRANCH'        TO wl_dynpfields-fieldname.
  APPEND wl_dynpfields TO tl_dynpfields.

  MOVE: 'WA_ZFIWRT0021-KUNNR'        TO wl_dynpfields-fieldname.
  APPEND wl_dynpfields TO tl_dynpfields.

  MOVE: 'WA_ZFIWRT0021-ANO'        TO wl_dynpfields-fieldname.
  APPEND wl_dynpfields TO tl_dynpfields.


  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = tl_dynpfields.

  READ TABLE tl_dynpfields INTO wl_dynpfields WITH KEY fieldname = 'WA_ZFIWRT0021-CONTRATO'.
  MOVE wl_dynpfields-fieldvalue TO wa_zfiwrt0021-contrato.
  "
  READ TABLE tl_dynpfields INTO wl_dynpfields WITH KEY fieldname = 'WA_ZFIWRT0021-BUKRS'.
  MOVE wl_dynpfields-fieldvalue TO wa_zfiwrt0021-bukrs.
  "
  READ TABLE tl_dynpfields INTO wl_dynpfields WITH KEY fieldname = 'WA_ZFIWRT0021-BRANCH'.
  MOVE wl_dynpfields-fieldvalue TO wa_zfiwrt0021-branch.
  "
  READ TABLE tl_dynpfields INTO wl_dynpfields WITH KEY fieldname = 'WA_ZFIWRT0021-KUNNR'.
  MOVE wl_dynpfields-fieldvalue TO wa_zfiwrt0021-kunnr.
  "
  READ TABLE tl_dynpfields INTO wl_dynpfields WITH KEY fieldname = 'WA_ZFIWRT0021-ANO'.
  MOVE wl_dynpfields-fieldvalue TO wa_zfiwrt0021-ano.

  IF wa_zfiwrt0021-contrato IS NOT INITIAL.
    w_contrato-sign = 'I'.
    w_contrato-option = 'EQ'.
    w_contrato-low = wa_zfiwrt0021-contrato.
    APPEND w_contrato  TO r_contrato.
  ENDIF.

  IF wa_zfiwrt0021-bukrs IS NOT INITIAL.
    w_bukrs-sign = 'I'.
    w_bukrs-option = 'EQ'.
    w_bukrs-low = wa_zfiwrt0021-bukrs.
    APPEND w_bukrs  TO r_bukrs.
  ENDIF.

  IF wa_zfiwrt0021-branch IS NOT INITIAL.
    w_branch-sign = 'I'.
    w_branch-option = 'EQ'.
    w_branch-low = wa_zfiwrt0021-branch.
    APPEND w_branch  TO r_branch.
  ENDIF.

  IF it_t0021[] IS NOT INITIAL.
    r_kunnr = VALUE #( FOR l IN it_t0021 (
    sign = 'I'
    option = 'EQ'
    low = |{ l-kunnr ALPHA = IN }|
    ) ).

    r_ano = VALUE #( FOR x IN it_t0021 (
    sign = 'I'
    option = 'EQ'
    low = |{ x-ano ALPHA = IN }|
    ) ).
  ENDIF.

  IF wa_zfiwrt0021-kunnr IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zfiwrt0021-kunnr
      IMPORTING
        output = wa_zfiwrt0021-kunnr.

    w_kunnr-sign = 'I'.
    w_kunnr-option = 'EQ'.
    w_kunnr-low = wa_zfiwrt0021-kunnr.
    APPEND w_kunnr  TO r_kunnr.
  ENDIF.



  IF wa_zfiwrt0021-ano IS NOT INITIAL.
    w_ano-sign = 'I'.
    w_ano-option = 'EQ'.
    w_ano-low = wa_zfiwrt0021-ano.
    APPEND w_ano  TO r_ano.
  ENDIF.

  SELECT tipo contrato bukrs branch kna1~kunnr kna1~name1 ano
     FROM zfiwrt0021
     INNER JOIN kna1 ON kna1~kunnr = zfiwrt0021~kunnr
     INTO TABLE tl_zfiw0021
     WHERE contrato IN r_contrato
     AND   bukrs    IN r_bukrs
     AND   branch   IN r_branch
     AND   zfiwrt0021~kunnr    IN r_kunnr
     AND   ano      IN r_ano.

  IF tl_zfiw0021[] IS NOT INITIAL. "bug close

    CALL SCREEN 0500 STARTING AT 010 1
                      ENDING  AT 90 10.

    IF ok-code NE '&SAIR'.
      REFRESH tl_dynpfields.
      MOVE: 'WA_ZFIWRT0021-CONTRATO'        TO wl_dynpfields-fieldname,
           wa_zfiwrt0021-contrato         TO wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.
      "
      MOVE: 'WA_ZFIWRT0021-BUKRS'        TO wl_dynpfields-fieldname,
             wa_zfiwrt0021-bukrs         TO wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.
      "
      MOVE: 'WA_ZFIWRT0021-BRANCH'        TO wl_dynpfields-fieldname,
            wa_zfiwrt0021-branch          TO wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.
      MOVE: 'WA_ZFIWRT0021-KUNNR'        TO wl_dynpfields-fieldname,
             wa_zfiwrt0021-kunnr         TO wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.
      "
      MOVE: 'WA_ZFIWRT0021-ANO'        TO wl_dynpfields-fieldname,
             wa_zfiwrt0021-ano         TO wl_dynpfields-fieldvalue.
      APPEND wl_dynpfields TO tl_dynpfields.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname     = sy-repid
          dynumb     = sy-dynnr
        TABLES
          dynpfields = tl_dynpfields.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '/00'
        EXCEPTIONS
          function_not_supported = 1.
    ENDIF.
  ENDIF.
  CLEAR vedit.
ENDFORM.
