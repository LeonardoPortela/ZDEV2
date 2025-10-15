*&---------------------------------------------------------------------*
*& Report  ZSDR0154
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zsdr0154 MESSAGE-ID zles.

CLASS lcl_alv_toolbar DEFINITION DEFERRED.
CLASS lcl_event_handler DEFINITION DEFERRED.

DATA: dg_splitter          TYPE REF TO cl_gui_splitter_container,
      ctl_cccontainer      TYPE REF TO cl_gui_container,
      ctl_cccontainer_html TYPE REF TO cl_gui_container,
      ctl_alv              TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog      TYPE lvc_t_fcat,
      gs_variant           TYPE disvariant,
      gs_layout            TYPE lvc_s_layo,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      obj_toolbarmanager   TYPE REF TO cl_alv_grid_toolbar_manager,
      lc_open_browser      TYPE char01,
      event_handler        TYPE REF TO lcl_event_handler.

*---------- Definition -----------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_column_id es_row_no.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.                    "lcl_event_handler DEFINITION

*---------- Implementation -------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
  METHOD handle_hotspot_click.
    PERFORM handle_hotspot_click USING es_row_no-row_id e_column_id-fieldname.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_double_click.
    PERFORM handle_double_click USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK

ENDCLASS.                    "lcl_event_handler

CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor  IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar          FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

CLASS lcl_alv_toolbar IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.

*    "Separador
    CLEAR ty_toolbar.
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CLEAR ty_toolbar.
    ty_toolbar-icon      = icon_execute_object.
    ty_toolbar-function  = 'CONSULTAR'.
    ty_toolbar-quickinfo = TEXT-004.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.

    CALL METHOD obj_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN 'CONSULTAR'.
        PERFORM seleciona_integracao.
    ENDCASE.

    ctl_alv->refresh_table_display( i_soft_refresh = abap_true ).

  ENDMETHOD. "zm_handle_user_command

ENDCLASS.                    "LCL_ALV_TOOLBAR_N55 IMPLEMENTATION

DATA: it_retorno   TYPE TABLE OF zsdt0325_inb_cliente,
      ok_code      TYPE sy-ucomm,
      it_mensagens TYPE TABLE OF zintegracao_alv,
      wa_mensagens TYPE zintegracao_alv.

SELECTION-SCREEN BEGIN OF BLOCK viag01 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS:
                  s_dtreg FOR wa_mensagens-dt_registro DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK viag01.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  PARAMETERS:
    p_pend  TYPE char1 RADIOBUTTON GROUP rb1 USER-COMMAND radio
                                DEFAULT 'X',
    p_final TYPE char1 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END   OF BLOCK b2.

AT SELECTION-SCREEN.

START-OF-SELECTION.
  PERFORM seleciona_integracao.

END-OF-SELECTION.

  CALL SCREEN 0001.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_INTEGRACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_integracao .
*** Inicio - Rubenilson  - 19.06.24 - #150257
  TYPES:
    BEGIN OF ty_dados_obrigatorios,
      grupo               TYPE ktokk,
      regiao              TYPE regio,
      ie                  TYPE stcd3,
      cnpj                TYPE stcd1,
      cpf                 TYPE stcd2,
      data_nasc           TYPE char10,
      nis_pis             TYPE kraus_cm,
      nome                TYPE name1,
      rntrc_antt          TYPE bahns,
      emissor_nota_fiscal TYPE char1,
    END OF ty_dados_obrigatorios .
  TYPES:
    BEGIN OF ty_dados_banco,
      id          TYPE char4,
      regiao      TYPE char2,
      chave_banco TYPE bu_bankk,
      conta       TYPE bu_bankn,
    END OF ty_dados_banco .
  TYPES:
    tb_empresas TYPE TABLE OF bukrs .

  DATA:
  tb_banco1 TYPE TABLE OF ty_dados_banco .

  TYPES:
    BEGIN OF ty_dados_gerais,
      rua             TYPE ad_street,
      numero          TYPE ad_hsnm1,
      cep             TYPE ad_pstcd1,
      pais            TYPE land1,
      cidade          TYPE ad_city1,
      bairro          TYPE ad_city2,
      estado          TYPE regio,
      fax             TYPE ad_fxnmbr1,
      email           TYPE ad_smtpadr,
      estado_civil    TYPE char1,
      regime_bens     TYPE char2,
      rg              TYPE char7,
      orgao_exp       TYPE char50,
      dados_bancarios LIKE tb_banco1,
    END OF ty_dados_gerais .


  DATA:
    BEGIN OF zde_data_request,
      parceiro        TYPE lifnr,
      consulta_netrin TYPE ty_dados_obrigatorios,
      dados_gerais    TYPE ty_dados_gerais,
      empresas        TYPE tb_empresas,
    END OF zde_data_request .

  DATA:
    BEGIN OF zde_data_request2,
      parceiro        TYPE lfa1-lifnr,
      grupo           TYPE lfa1-ktokk,
      cpf             TYPE lfa1-stcd2,
      cnpj            TYPE lfa1-stcd1,
      Nome            TYPE lfa1-name1,
      Rua             TYPE adrc-street,
      numero          TYPE adrc-house_num1,
      bairro          TYPE adrc-city2,
      cep             TYPE adrc-post_code1,
      cidade          TYPE adrc-city1,
      regiao          TYPE adrc-region,
      pais            TYPE adrc-country,
      telefone        TYPE adrc-tel_number,
      email           TYPE adr6-smtp_addr,
      ie              TYPE lfa1-stcd3,
      dados_bancarios LIKE tb_banco1,
      empresas        TYPE tb_empresas,
    END OF zde_data_request2 .
*** Fim - Rubenilson  - 19.06.24 - #150257

  DATA: wa_data_retorno  TYPE zsdt0325_inb_cliente,
        lit_data_inbound TYPE zsdt0325_inbcli_t,
        lt_data_inbound2 LIKE zde_data_request,"Rubenilson  - 19.06.24 - #150257
        lt_data_inbound3 LIKE zde_data_request2,"Rubenilson  - 19.06.24 - #150257
        lc_data_view     TYPE string.

  CLEAR: it_retorno[], it_retorno, lit_data_inbound[], it_mensagens[].

  SELECT * INTO TABLE @DATA(it_mensagens_banco)
     FROM zintegracao
    WHERE id_interface  EQ '156'
      AND dt_registro   IN @s_dtreg.
  "AND ck_retornou     EQ 'X'.

  IF p_final = 'X'.
    DELETE it_mensagens_banco[] WHERE ck_integrado <> 'X'.
  ELSE.
    DELETE it_mensagens_banco[] WHERE ck_integrado = 'X'.
  ENDIF.

  LOOP AT it_mensagens_banco INTO DATA(wa_mensagens).
    TRY .
        zcl_integracao=>zif_integracao~get_instance(
          )->get_msg_alv( EXPORTING i_integracao = wa_mensagens IMPORTING e_integracao_alv = DATA(e_integracao_alv)
          ).
        APPEND e_integracao_alv TO it_mensagens.
      CATCH zcx_integracao INTO DATA(ex_erro).
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  SORT it_mensagens BY dthr_registro DESCENDING.

  LOOP AT it_mensagens INTO DATA(lwa_mensagens).
    CLEAR: lit_data_inbound[].

    TRY .
        zcl_integracao=>zif_integracao~get_instance(
         )->set_registro( i_id_integracao = lwa_mensagens-id_integracao
         )->get_registro( IMPORTING e_integracao = DATA(e_integracao)
         )->free(
         ).
        lc_data_view = e_integracao-ds_body.

        CHECK lc_data_view IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lc_data_view CHANGING data = lit_data_inbound ).

        SELECT *
           FROM  zsdt0327tx
         INTO TABLE @DATA(lit_integra_msg)
         WHERE id_integracao = @lwa_mensagens-id_integracao.

        LOOP AT lit_data_inbound INTO DATA(lwa_data_inbound).

          "READ TABLE lit_integra_msg INTO DATA(lwa_integra_msg) WITH KEY  id_origem     = lwa_data_inbound-idorigem.

          LOOP AT lit_integra_msg INTO DATA(lwa_integra_msg) WHERE  id_origem     = lwa_data_inbound-idorigem.
            IF lwa_integra_msg IS NOT INITIAL.
              MOVE-CORRESPONDING lwa_data_inbound TO wa_data_retorno.
              wa_data_retorno-status        = lwa_mensagens-ico_st_integrado.
              wa_data_retorno-id_integracao = lwa_mensagens-id_integracao.
              wa_data_retorno-log_mensagem  = lwa_integra_msg-msg_processamento.
              wa_data_retorno-idpartnersap  = lwa_integra_msg-id_cli_processado.
              wa_data_retorno-dt_registro   = lwa_integra_msg-dt_registro.
              wa_data_retorno-hr_registro   = lwa_integra_msg-hr_registro.
              wa_data_retorno-us_registro   = lwa_integra_msg-us_registro.

              APPEND wa_data_retorno TO it_retorno.
              CLEAR: wa_data_retorno, lwa_integra_msg.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

      CATCH zcx_integracao INTO DATA(ex_integra).
        ex_integra->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO DATA(ex_error).
        ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

*** Inicio - Rubenilson  - 19.06.24 - #150257
*** Integração Softex

  SELECT * INTO TABLE it_mensagens_banco
     FROM zintegracao
    WHERE id_interface  EQ '219'
      AND dt_registro   IN s_dtreg.

  IF p_final = 'X'.
    DELETE it_mensagens_banco[] WHERE ck_integrado <> 'X'.
  ELSE.
    DELETE it_mensagens_banco[] WHERE ck_integrado = 'X'.
  ENDIF.

  LOOP AT it_mensagens_banco INTO wa_mensagens.
    TRY .
        zcl_integracao=>zif_integracao~get_instance(
          )->get_msg_alv( EXPORTING i_integracao = wa_mensagens IMPORTING e_integracao_alv = e_integracao_alv
          ).
        APPEND e_integracao_alv TO it_mensagens.
      CATCH zcx_integracao INTO ex_erro.
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  SORT it_mensagens BY dthr_registro DESCENDING.

  LOOP AT it_mensagens INTO lwa_mensagens.
    CLEAR: lit_data_inbound[].

    TRY .
        zcl_integracao=>zif_integracao~get_instance(
         )->set_registro( i_id_integracao = lwa_mensagens-id_integracao
         )->get_registro( IMPORTING e_integracao = e_integracao
         )->free(
         ).
        lc_data_view = e_integracao-ds_body.

        CHECK lc_data_view IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lc_data_view CHANGING data = lt_data_inbound2 ).

        SELECT *
           FROM  zsdt0327tx
         INTO TABLE lit_integra_msg
         WHERE id_integracao = lwa_mensagens-id_integracao.

        LOOP AT lit_integra_msg INTO lwa_integra_msg.
          IF lwa_integra_msg IS NOT INITIAL.

            MOVE-CORRESPONDING  lt_data_inbound2-dados_gerais    TO wa_data_retorno.
            MOVE-CORRESPONDING  lt_data_inbound2-consulta_netrin TO wa_data_retorno.

            wa_data_retorno-status        = lwa_mensagens-ico_st_integrado.
            wa_data_retorno-id_integracao = lwa_mensagens-id_integracao.
            wa_data_retorno-idorigem      = lwa_integra_msg-id_origem.
            wa_data_retorno-origemcadastro = lwa_integra_msg-origem_cadastro.
            wa_data_retorno-grupoconta    = lt_data_inbound2-consulta_netrin-grupo.
            wa_data_retorno-log_mensagem  = lwa_integra_msg-msg_processamento.
            wa_data_retorno-idpartnersap  = lwa_integra_msg-id_cli_processado.
            wa_data_retorno-dt_registro   = lwa_integra_msg-dt_registro.
            wa_data_retorno-hr_registro   = lwa_integra_msg-hr_registro.
            wa_data_retorno-us_registro   = lwa_integra_msg-us_registro.

            APPEND wa_data_retorno TO it_retorno.
            CLEAR: wa_data_retorno, lwa_integra_msg.
          ENDIF.
        ENDLOOP.

      CATCH zcx_integracao INTO ex_integra.
        ex_integra->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO ex_error.
        ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

*** Integração Linkana

  SELECT * INTO TABLE it_mensagens_banco
     FROM zintegracao
    WHERE id_interface  EQ '221'
      AND dt_registro   IN s_dtreg.

  IF p_final = 'X'.
    DELETE it_mensagens_banco[] WHERE ck_integrado <> 'X'.
  ELSE.
    DELETE it_mensagens_banco[] WHERE ck_integrado = 'X'.
  ENDIF.

  LOOP AT it_mensagens_banco INTO wa_mensagens.
    TRY .
        zcl_integracao=>zif_integracao~get_instance(
          )->get_msg_alv( EXPORTING i_integracao = wa_mensagens IMPORTING e_integracao_alv = e_integracao_alv
          ).
        APPEND e_integracao_alv TO it_mensagens.
      CATCH zcx_integracao INTO ex_erro.
        ex_erro->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDLOOP.

  SORT it_mensagens BY dthr_registro DESCENDING.

  LOOP AT it_mensagens INTO lwa_mensagens.
    CLEAR: lit_data_inbound[].

    TRY .
        zcl_integracao=>zif_integracao~get_instance(
         )->set_registro( i_id_integracao = lwa_mensagens-id_integracao
         )->get_registro( IMPORTING e_integracao = e_integracao
         )->free(
         ).
        lc_data_view = e_integracao-ds_body.

        CHECK lc_data_view IS NOT INITIAL.

        /ui2/cl_json=>deserialize( EXPORTING json = lc_data_view CHANGING data = lt_data_inbound3 ).

        SELECT *
           FROM  zsdt0327tx
         INTO TABLE lit_integra_msg
         WHERE id_integracao = lwa_mensagens-id_integracao.

        LOOP AT lit_integra_msg INTO lwa_integra_msg.
          IF lwa_integra_msg IS NOT INITIAL.

            MOVE-CORRESPONDING  lt_data_inbound3   TO wa_data_retorno.

            wa_data_retorno-status        = lwa_mensagens-ico_st_integrado.
            wa_data_retorno-id_integracao = lwa_mensagens-id_integracao.
            wa_data_retorno-idorigem      = lwa_integra_msg-id_origem.
            wa_data_retorno-origemcadastro = lwa_integra_msg-origem_cadastro.
            wa_data_retorno-grupoconta    = lt_data_inbound3-grupo.
            wa_data_retorno-log_mensagem  = lwa_integra_msg-msg_processamento.
            wa_data_retorno-idpartnersap  = lwa_integra_msg-id_cli_processado.
            wa_data_retorno-dt_registro   = lwa_integra_msg-dt_registro.
            wa_data_retorno-hr_registro   = lwa_integra_msg-hr_registro.
            wa_data_retorno-us_registro   = lwa_integra_msg-us_registro.

            APPEND wa_data_retorno TO it_retorno.
            CLEAR: wa_data_retorno, lwa_integra_msg.
          ENDIF.
        ENDLOOP.

      CATCH zcx_integracao INTO ex_integra.
        ex_integra->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_error INTO ex_error.
        ex_error->zif_error~published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
*** Fim - Rubenilson  - 19.06.24 - #150257
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001_exit INPUT.

  PERFORM sair_0001.

  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.
  SET PF-STATUS 'PF0001'.
  SET TITLEBAR 'TL0001'.

  IF dg_splitter IS INITIAL.

    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = cl_gui_container=>screen0 "CTL_CCCONTAINER
        rows    = 1
        columns = 1. "2

    ctl_cccontainer      = dg_splitter->get_container( EXPORTING row = 1 column = 1 ).

    CREATE OBJECT ctl_alv
      EXPORTING
        i_parent = ctl_cccontainer.

    PERFORM fill_it_fieldcatalog.

    PERFORM fill_gs_variant.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = ctl_alv.
*
    SET HANDLER obg_toolbar->on_toolbar FOR ctl_alv.
    SET HANDLER obg_toolbar->handle_user_command FOR ctl_alv.

    CALL METHOD ctl_alv->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
        is_variant      = gs_variant
        i_save          = 'A'
        "IT_EXCEPT_QINFO = ZCL_INTEGRACAO=>ZIF_INTEGRACAO~GET_QINFO_ALV( )
      CHANGING
        it_fieldcatalog = it_fieldcatalog
        it_outtab       = it_retorno[].

    CREATE OBJECT event_handler.
    SET HANDLER event_handler->handle_hotspot_click FOR ctl_alv.
    SET HANDLER event_handler->handle_double_click  FOR ctl_alv.

  ENDIF.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZSDT0325_INB_CLIENTE'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

  LOOP AT it_fieldcatalog ASSIGNING <fs_cat>.

    CASE <fs_cat>-fieldname .
      WHEN 'STATUS'.
        <fs_cat>-col_pos = '1'.
        <fs_cat>-coltext = 'Status'.
        <fs_cat>-just    = 'C'.
      WHEN 'ID_INTEGRACAO'.
        <fs_cat>-col_pos = '2'.
        <fs_cat>-coltext = 'Id Integração'.
        <fs_cat>-hotspot = 'X'.
      WHEN 'LOG_MENSAGEM'.
        <fs_cat>-col_pos = '3'.
        <fs_cat>-coltext = 'Log. Processamento'.
      WHEN 'IDORIGEM' .
        <fs_cat>-col_pos = '4'.
        <fs_cat>-coltext = 'Id Origem'.
      WHEN 'ORIGEMCADASTRO'.
        <fs_cat>-col_pos = '5'.
        <fs_cat>-coltext = 'Origem Cadastro'.
      WHEN 'GRUPOCONTA'.
        <fs_cat>-col_pos = '6'.
        <fs_cat>-coltext = 'Grupo de Conta'.
      WHEN 'EMPRESA'.
        <fs_cat>-col_pos = '7'.
        <fs_cat>-coltext = 'Empresa'.
      WHEN 'SETORATIVIDADE'.
        <fs_cat>-col_pos = '8'.
        <fs_cat>-coltext = 'Setor Atividade'.
      WHEN 'NOME'.
        <fs_cat>-col_pos = '9'.
        <fs_cat>-coltext = 'Nome'.
      WHEN 'GENERO'.
        <fs_cat>-col_pos = '10'.
        <fs_cat>-coltext = 'Gênero'.
      WHEN 'DATANASCIMENTO'.
        <fs_cat>-col_pos = '11'.
        <fs_cat>-coltext = 'Data de Nascimento'.
      WHEN 'CNPJ'.
        <fs_cat>-col_pos = '12'.
        <fs_cat>-coltext = 'CNPJ'.
      WHEN 'CPF'.
        <fs_cat>-col_pos = '13'.
        <fs_cat>-coltext = 'CPF'.
      WHEN 'INSCESTADUAL'.
        <fs_cat>-col_pos = '14'.
        <fs_cat>-coltext = 'Insc.Estadual'.
      WHEN 'INSCMUNICIPAL'.
        <fs_cat>-col_pos = '15'.
        <fs_cat>-coltext = 'Insc.Municipal'.
      WHEN 'RG'.
        <fs_cat>-col_pos = '16'.
        <fs_cat>-coltext = 'RG'.
      WHEN 'ENDERECO'.
        <fs_cat>-col_pos = '17'.
        <fs_cat>-coltext = 'Edereço'.
      WHEN 'NRENDERECO'.
        <fs_cat>-col_pos = '18'.
        <fs_cat>-coltext = 'Nº'.
      WHEN 'CIDADE'.
        <fs_cat>-col_pos = '19'.
        <fs_cat>-coltext = 'Cidade'.
      WHEN 'BAIRRO'.
        <fs_cat>-col_pos = '20'.
        <fs_cat>-coltext = 'Bairro'.
      WHEN 'UF'.
        <fs_cat>-col_pos = '21'.
        <fs_cat>-coltext = 'UF'.
      WHEN 'CEP'.
        <fs_cat>-col_pos = '22'.
        <fs_cat>-coltext = 'CEP'.
      WHEN 'PAIS'.
        <fs_cat>-col_pos = '23'.
        <fs_cat>-coltext = 'Pais'.
      WHEN 'TELEFONEFIXO'.
        <fs_cat>-col_pos = '24'.
        <fs_cat>-coltext = 'Tel.Fixo'.
      WHEN 'TELEFONEMOVEL'.
        <fs_cat>-col_pos = '25'.
        <fs_cat>-coltext = 'Tel.Móvel'.
      WHEN 'EMAIL'.
        <fs_cat>-col_pos = '26'.
        <fs_cat>-coltext = 'Email'.
      WHEN 'IDPARTNERSAP'.
        <fs_cat>-col_pos = '27'.
        <fs_cat>-coltext = 'Código Cliente SAP'.
      WHEN 'DT_REGISTRO'.
        <fs_cat>-col_pos = '28'.
        <fs_cat>-coltext = 'Data Log'.
      WHEN 'HR_REGISTRO'.
        <fs_cat>-col_pos = '29'.
        <fs_cat>-coltext = 'Hora Log'.
      WHEN 'US_REGISTRO'.
        <fs_cat>-col_pos = '30'.
        <fs_cat>-coltext = 'Usuário Log'.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant .

  gs_variant-report      = sy-repid.
  gs_variant-handle      = '0001'.
  gs_variant-log_group   = abap_false.
  gs_variant-username    = abap_false.
  gs_variant-variant     = abap_false.
  gs_variant-text        = abap_false.
  gs_variant-dependvars  = abap_false.

  gs_layout-sel_mode     = 'A'.
  gs_layout-info_fname   = 'LINE_COLOR'.
  gs_layout-stylefname   = 'STYLE'.
  gs_layout-ctab_fname   = 'COLOR_CELL'.
  gs_layout-zebra        = abap_false.
  gs_layout-cwidth_opt   = abap_true.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SAIR_0001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sair_0001 .

  CLEAR: event_handler, obg_toolbar.

  IF lc_open_browser = abap_true.
    cl_abap_browser=>close_browser( ).
  ENDIF.

  IF ctl_alv IS NOT INITIAL.
    ctl_alv->free( ).
  ENDIF.
  CLEAR: ctl_alv.

  IF ctl_cccontainer IS NOT INITIAL.
    ctl_cccontainer->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer.

  IF ctl_cccontainer_html IS NOT INITIAL.
    ctl_cccontainer_html->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_html.

  IF dg_splitter IS NOT INITIAL.
    dg_splitter->free( ).
  ENDIF.
  CLEAR: dg_splitter.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM handle_hotspot_click
         USING VALUE(row_id)    LIKE lvc_s_roid-row_id
               VALUE(fieldname) LIKE lvc_s_col-fieldname.

  READ TABLE it_retorno INDEX row_id INTO DATA(wa_ret).

  CASE fieldname.
    WHEN 'ID_INTEGRACAO'.
      CHECK wa_ret-id_integracao IS NOT INITIAL.
      PERFORM chamar_integracao USING wa_ret-id_integracao.
  ENDCASE.

ENDFORM.                    " HANDLE_HOTSPOT_CLICK

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  IF p_row-rowtype IS INITIAL.
    READ TABLE it_retorno INDEX p_row-index INTO DATA(wa_ret).
    PERFORM mostrar_mensagens USING wa_ret.
  ENDIF.

ENDFORM.                    " HANDLE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_MENSAGENS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0185  text
*----------------------------------------------------------------------*
FORM mostrar_mensagens  USING p_wa_ret TYPE zsdt0325_inb_cliente.


*  IF lc_open_browser = abap_true.
*    cl_abap_browser=>close_browser( ).
*    lc_open_browser = abap_false.
*  ENDIF.
*
*  lc_open_browser = abap_true.
*
*  cl_abap_browser=>show_html(
*   EXPORTING
*     html_string = zcl_integracao_ord_carrega=>get_txt_html_viagem( EXPORTING i_viagem_id = p_wa_0185-viagem_id )
*     modal       = abap_false
*     format      = cl_abap_browser=>landscape
*     size        = cl_abap_browser=>small
*     container   = ctl_cccontainer_html ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMAR_INTEGRACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_0185_ID_INTEGRACAO_APROVAR  text
*----------------------------------------------------------------------*
FORM chamar_integracao  USING p_id_integracao TYPE zde_id_integracao.

  CALL FUNCTION 'ZLES_CARGUERO_0004'
    EXPORTING
      id_integracao = p_id_integracao.

ENDFORM.
