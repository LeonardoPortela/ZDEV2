*&---------------------------------------------------------------------*
*& Report  ZLESR0144
*&
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Gera  arquivo XML ANTAQ                                 *
* Transação..: ZLES                                                       *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
**23.04.20|JALEXANDRE |         | Gera  arquivo XML ANTAQ              *
*----------------------------------------------------------------------*
REPORT zlesr0144 MESSAGE-ID zles.

* =====================================================================
* Tabelas Transparentes
* =====================================================================
TABLES: zlest0061, icon, zlest0201, t001w, zlest0053, marc, zlest0056.

* =====================================================================
* Tipos
* =====================================================================
TYPES: BEGIN OF y_saida,
         status(25)          TYPE c, "ICON-ID,
         nr_atracacao(15)    TYPE c,
         apelido             TYPE zlest0053-apelido,
         dt_descarga_fim     TYPE zlest0061-dt_descarga_fim,
         hr_descarga_fim     TYPE zlest0061-hr_descarga_fim,
         bukrs               TYPE zlest0061-bukrs,
         werks               TYPE zlest0061-werks,
         ano_viagem          TYPE zlest0061-ano_viagem,
         nr_viagem           TYPE zlest0061-nr_viagem,
         nome_emb            TYPE zlest0061-nome_emb,
         cod_material        TYPE zlest0061-cod_material,
         dt_fatura           TYPE zlest0061-dt_fatura,
         peso_vinculado      TYPE zlest0061-peso_vinculado,
         peso_chegada        TYPE zlest0061-peso_chegada,
         cl_codigo           TYPE zlest0061-cl_codigo,
         safra               TYPE zlest0061-safra,
         docnum              TYPE zlest0061-docnum,
         nfenum              TYPE j_1bnfdoc-nfenum,
         dt_chegada          TYPE zlest0061-dt_chegada,
         hr_chegada          TYPE zlest0061-hr_chegada,
         dt_chegada_terminal TYPE zlest0061-dt_chegada_terminal,
         dt_descarga_ini     TYPE zlest0061-dt_descarga_ini,
         hr_descarga_ini     TYPE zlest0061-hr_descarga_ini,
         nr_movimento        TYPE zlest0203-nr_movimento,
         codigo_carga        TYPE marc-steuc,
       END OF y_saida.

TYPES: BEGIN OF y_saida_9001,
         inst_portuaria   TYPE zlest0201-inst_portuaria,
         inst_port_desc   TYPE t001w-name1,
         tup_inst_port    TYPE zlest0201-tup_inst_port,
         cnpj_inst_port   TYPE zlest0201-cnpj_inst_port,
         local_atracacao  TYPE zlest0201-local_atracacao,
         cnpj_ag_maritimo TYPE zlest0201-cnpj_ag_maritimo,
         cnpj_armador     TYPE zlest0201-cnpj_armador,
         cnpj_operador    TYPE zlest0201-cnpj_operador,
         nacional_armador TYPE zlest0201-nacional_armador,
         usuario_criacao  TYPE zlest0201-usuario_criacao,
         data_criacao     TYPE zlest0201-data_criacao,
         hr_criacao       TYPE zlest0201-hr_criacao,
         us_modif         TYPE zlest0201-us_modif,
         dt_modif         TYPE zlest0201-dt_modif,
         hr_modif         TYPE zlest0201-hr_modif,
       END OF y_saida_9001.

TYPES: BEGIN OF y_marc,
         matnr TYPE marc-matnr,
         werks TYPE marc-werks,
         steuc TYPE marc-steuc,
       END OF y_marc.

TYPES: BEGIN OF y_xml,
         nr_atracacao    TYPE zlest0202-nr_atracacao,
         dt_descarga_fim TYPE zlest0202-dt_descarga_fim,
         printd          TYPE zlest0202-printd,
         nr_movimento    TYPE zlest0202-nr_movimento,
       END OF y_xml.

* =====================================================================
* Tabela Interna
* =====================================================================
* Tela 9000
DATA: t_zlest0061 TYPE TABLE OF zlest0061,
      t_doc       TYPE TABLE OF j_1bnfdoc,
      t_saida     TYPE TABLE OF y_saida,
      t_xml       TYPE TABLE OF y_xml.

* Tela 9001
DATA: t_zlest0201  TYPE TABLE OF zlest0201,
      t_saida_9001 TYPE TABLE OF y_saida_9001.

* Tela 9002
DATA: t_parametros TYPE TABLE OF y_saida_9001.

* Todas as Telas
DATA: t_sort      TYPE          lvc_t_sort.

DATA: t_zlest0053 TYPE TABLE OF zlest0053,
      t_marc      TYPE TABLE OF y_marc,
      t_zlest0056 TYPE TABLE OF zlest0056,
      t_carga     TYPE zlesmessage1_carga_tab,
      t_bdcdata   TYPE STANDARD TABLE OF bdcdata.

* =====================================================================
* Estruturas
* =====================================================================
DATA: w_zlest0201  LIKE LINE OF t_zlest0201,
      w_saida      LIKE LINE OF t_saida,
      w_saida_9001 LIKE LINE OF t_saida_9001,
      w_parametros LIKE LINE OF t_parametros,
      w_zlest0053  LIKE LINE OF t_zlest0053,
      w_xml        LIKE LINE OF t_xml,
      w_abap       TYPE zlesmessage1.


* =====================================================================
*Definição de Icones
* =====================================================================
DATA: BEGIN OF w_tela,
        subtela(20) TYPE c        VALUE  'SUB',
        programa    TYPE sy-repid VALUE  'ZLESR0144',
        tela        TYPE sy-dynnr VALUE  9000,
      END OF w_tela.

* =====================================================================
*Definição de Icones
* =====================================================================
DATA: BEGIN OF w_icones,

        icon_complete(50)       TYPE c,
        icon_generate(50)       TYPE c,
        icon_system_save(50)    TYPE c,
        icon_table_settings(50) TYPE c,
        icon_display(50)        TYPE c,
        icon_create(50)         TYPE c,
        icon_change(50)         TYPE c,
        icon_delete(50)         TYPE c,
        icon_checked(50)        TYPE c,
        icon_system_paste(50)   TYPE c,
      END OF w_icones.

* =====================================================================
* Ranges
* =====================================================================

* =====================================================================
* Constantes
* =====================================================================
CONSTANTS: BEGIN OF w_constantes,
             btn_gerarxml(20)           TYPE c VALUE 'GERARXML',
             btn_gravarxml(20)          TYPE c VALUE 'GRAVARXML',
             btn_showxml(20)            TYPE c VALUE 'SHOWXML',
             btn_parametros(20)         TYPE c VALUE 'PARAMETROS',
             btn_display(20)            TYPE c VALUE 'DISPLAY',
             btn_create(20)             TYPE c VALUE 'CREATE',
             btn_change(20)             TYPE c VALUE 'CHANGE',
             btn_delete(20)             TYPE c VALUE 'DELETE',

             inst_port_1002(4)          TYPE c VALUE '1002',
             navegacao_1                TYPE c VALUE '1',
             tipo_operacao_1            TYPE c VALUE '1',
             po_embarque_1003(4)        TYPE c VALUE '1003',
             trig_porto_orig_br(2)      TYPE c VALUE 'BR',
             big_pais_orig_pvh(3)       TYPE c VALUE 'PVH',

             big_pais_orig_stm(3)       TYPE c VALUE 'STM',         "*-IR209574-19.11.2024-#158940-JT-inicio

             po_embarque_0161(4)        TYPE c VALUE '0161',
             cod_tupori_brro013(7)      TYPE c VALUE 'BRRO013',

             po_embarque_1001(4)        TYPE c VALUE '1001',
             cod_tupori_bram029(7)      TYPE c VALUE 'BRAM029',

             po_embarque_3904(4)        TYPE c VALUE '3904',        "*-IR209574-19.11.2024-#158940-JT-inicio
             cod_tupori_brpa038(7)      TYPE c VALUE 'BRPA038',     "*-IR209574-19.11.2024-#158940-JT-inicio
             cod_tupori_bram091(7)      TYPE c VALUE 'BRAM091',     "*-IR224070-19.02.2025-#167410-JT-inicio
             po_embarque_0000229649(10) TYPE c VALUE '0000229649',
             cod_tupori_bram079(7)      TYPE c VALUE 'BRAM079',

             po_embarque_0000188702(10) TYPE c VALUE '0000188702',  "*-IR209574-19.11.2024-#158940-JT-inicio
             po_embarque_0000538943(10) TYPE c VALUE '0000538943',  "*-IR209574-19.11.2024-#158940-JT-inicio
             po_embarque_0000186993(10) TYPE c VALUE '0000186993',  "*-IR224070-19.02.2025-#167410-JT-inicio
             po_destino_1002(4)         TYPE c VALUE '1002',
             po_tupdest_bram014(7)      TYPE c VALUE 'BRAM014',
             cod_area_dest_a1(4)        TYPE c VALUE '0001',
             peso_1000(4)               TYPE c VALUE '1000',
             cod_op_carga_18(2)         TYPE c VALUE '18',
             natureza_carga_1           TYPE c VALUE '1',
             sentido_1                  TYPE c VALUE '1',
             imon_0                     TYPE c VALUE '0',

             10minutos                  TYPE t VALUE '001000',
             5minutos                   TYPE t VALUE '000500',
             subtrair                   TYPE c VALUE '-',
             adicionar                  TYPE c VALUE  '+',
           END OF w_constantes.
* =====================================================================
* Variável Global
* =====================================================================

* =====================================================================
* FIELD-SYMBOLS
* =====================================================================
FIELD-SYMBOLS: <fs_fcat>  TYPE lvc_s_fcat,
               <fs_saida> LIKE LINE OF t_saida.

* =====================================================================
* CLASS DEFINITION
* =====================================================================
CLASS       lcl_event_receiver DEFINITION FINAL.

  PUBLIC SECTION.
*---Method to handel hotspot
    METHODS :
      handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id.

*---Method to handel toolbar
    METHODS :
      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.

    METHODS :
      handle_toolbar_9001
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive.

*---Method to handel user_command
    METHODS :
      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    METHODS :
      handle_user_command_9001 FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS .                    "LCL_EVENT_RECEIVER DEFINITION

* =====================================================================
* Instância
* =====================================================================
DATA: v_grid            TYPE REF TO cl_gui_alv_grid.        "#EC NEEDED
DATA: v_event_receiver  TYPE REF TO lcl_event_receiver.     "#EC NEEDED

DATA: v_grid_9001      TYPE REF TO cl_gui_alv_grid.         "#EC NEEDED
DATA: v_event_receiver_9001 TYPE REF TO lcl_event_receiver. "#EC NEEDED

DATA: v_ucomm    TYPE sy-ucomm,
      v_xml      TYPE xstring,
      v_printd   TYPE c,
      v_string   TYPE string,
      v_nr_movim TYPE zdeles_nr_movim,
      v_disabled TYPE c.

* =====================================================================
* CLASS IMPLEMENTATION
* =====================================================================
CLASS lcl_event_receiver IMPLEMENTATION.

*-----Logic to handle the HOTSPOT click
  METHOD handle_hotspot_click.
*---To handel hotspot in the firstlist
    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.

  ENDMETHOD.                    "HANDEL_HOTSPOT_CLICK

*-----Logic to handle the ToolBar
  METHOD handle_toolbar.
    PERFORM zf_adiciona_botoes_header USING e_object.
    PERFORM zf_elimina_botoes_header  USING e_object.
  ENDMETHOD.

  METHOD handle_toolbar_9001.
    PERFORM zf_elimina_botoes_9001  USING e_object.
    PERFORM zf_adiciona_botoes_9001 USING e_object.
  ENDMETHOD.

  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN w_constantes-btn_gerarxml.
        PERFORM zf_gerar_xml.
      WHEN w_constantes-btn_gravarxml.
        PERFORM zf_gravar_xml.
      WHEN w_constantes-btn_showxml.
        PERFORM zf_visualizar_xml.
      WHEN w_constantes-btn_parametros.
        CALL SCREEN 9001."9200.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD handle_user_command_9001.

    DATA: t_index_rows TYPE lvc_t_row,                      "#EC NEEDED
          t_row_no     TYPE lvc_t_roid,
          w_row_no     LIKE LINE OF t_row_no,
          w_saida      LIKE LINE OF t_saida,
          l_linhas     TYPE sy-tabix.

    CLEAR v_ucomm.
    v_ucomm = e_ucomm.

    CLEAR w_parametros.
    IF e_ucomm EQ w_constantes-btn_create.

      sy-ucomm = e_ucomm.
*      IF V_GRID_9001 IS BOUND.
*        CALL METHOD V_GRID_9001->FREE.
*      ENDIF.
      CALL SCREEN 9002.

    ELSE.

      CALL METHOD v_grid_9001->get_selected_rows
        IMPORTING
          et_index_rows = t_index_rows
          et_row_no     = t_row_no.

      DESCRIBE TABLE t_row_no LINES l_linhas.
      IF l_linhas = 0.
        "Selecionar uma linha.
        MESSAGE i000 WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
        RETURN.
      ELSEIF l_linhas > 1.
        "Selecionar apenas uma linha
        MESSAGE i000 WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      LOOP AT t_row_no INTO w_row_no.

        READ TABLE t_saida_9001 INTO w_parametros INDEX w_row_no-row_id.

        IF sy-subrc IS INITIAL.

* Preenche texto local de Negócio
          PERFORM zf_busca_inst_port_desc.

          CASE e_ucomm.

            WHEN w_constantes-btn_change.
*              IF V_GRID_9001 IS BOUND.
*                CALL METHOD V_GRID_9001->FREE.
*              ENDIF.
              CALL SCREEN 9002.

            WHEN w_constantes-btn_display.
              IF w_parametros-inst_portuaria IS NOT INITIAL.
*                IF V_GRID_9001 IS BOUND.
*                  CALL METHOD V_GRID_9001->FREE.
*                ENDIF.
                CALL SCREEN 9002.
              ENDIF.
            WHEN w_constantes-btn_delete.
              PERFORM zf_elimina_registro.
            WHEN OTHERS.
          ENDCASE.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

* =====================================================================
* Parâmetros de seleção
* =====================================================================

SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(19) TEXT-002 FOR FIELD p_descfi.
    SELECTION-SCREEN POSITION 33.
    PARAMETERS: p_descfi LIKE zlest0061-dt_descarga_fim OBLIGATORY.
    SELECTION-SCREEN POSITION 48.
    SELECTION-SCREEN COMMENT 48(5) TEXT-003 FOR FIELD p_descfi.
    PARAMETERS: p_descff LIKE zlest0061-dt_descarga_fim OBLIGATORY.

  SELECTION-SCREEN END OF LINE.

  SELECT-OPTIONS:
             s_bukrs FOR zlest0061-bukrs           NO-EXTENSION NO INTERVALS,
             s_werks FOR zlest0061-werks           NO-EXTENSION NO INTERVALS,
             s_anov  FOR zlest0061-ano_viagem      NO-EXTENSION NO INTERVALS,
             s_nrv   FOR zlest0061-nr_viagem       NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK  bl1.

*SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-009.
*PARAMETERS: P_LAYOUT      TYPE DISVARIANT-VARIANT MODIF ID T1 DEFAULT '/STD' NO-DISPLAY. "Layout
*SELECTION-SCREEN END OF BLOCK B2.

* =====================================================================
* AT SELECTION SCREEN ON specific field.
* =====================================================================
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_LAYOUT.
*  PERFORM ZF_CARREGA_LAYOUT_ALV.

* =====================================================================
*START-OF-SELECTION.
* =====================================================================
START-OF-SELECTION.

  PERFORM zf_valida_periodo CHANGING v_disabled .
  PERFORM zf_limpar_globais    .
  PERFORM zf_selecionar_dados  .
  PERFORM zf_montar_saida      .
  PERFORM zf_montar_monitor    .
*&---------------------------------------------------------------------*
*&      Form  ZF_LIMPAR_GLOBAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_limpar_globais .

* Tabelas Internas
  REFRESH: t_saida, t_saida_9001, t_zlest0061, t_zlest0201,
            t_doc, t_parametros, t_zlest0053, t_marc, t_zlest0056,
            t_carga.

* Estruturas
  CLEAR: w_zlest0201, w_saida_9001, w_parametros.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_selecionar_dados .

* Verifica se data fim está preenchida
  SELECT * FROM zlest0061
    INTO TABLE t_zlest0061
     WHERE bukrs IN s_bukrs
       AND werks IN s_werks
       AND ano_viagem IN s_anov
       AND nr_viagem  IN s_nrv
       AND dt_descarga_fim >= p_descfi
       AND dt_descarga_fim <= p_descff.

  IF t_zlest0061[] IS NOT INITIAL.

* Busca Número de CTE
    SELECT * FROM j_1bnfdoc
      INTO TABLE t_doc
      FOR ALL ENTRIES IN t_zlest0061
      WHERE docnum = t_zlest0061-docnum.

* Cadastro rebocadores/empurradores e barcaças
    SELECT * FROM zlest0053
      INTO TABLE t_zlest0053
      FOR ALL ENTRIES IN t_zlest0061
      WHERE bukrs = t_zlest0061-bukrs
        AND nome  = t_zlest0061-nome_emb.

* Dados de centro para material
    SELECT matnr werks steuc FROM marc
      INTO TABLE t_marc
      FOR ALL ENTRIES IN t_zlest0061
      WHERE matnr = t_zlest0061-cod_material
        AND werks = t_zlest0061-werks
        AND steuc <> space.

    SELECT * FROM zlest0056
      INTO TABLE t_zlest0056
      FOR ALL ENTRIES IN t_zlest0061
      WHERE bukrs      = t_zlest0061-bukrs
        AND werks      = t_zlest0061-werks
        AND nr_viagem	 = t_zlest0061-nr_viagem
        AND ano_viagem = t_zlest0061-ano_viagem.

* Busca XML de Atracação
    SELECT nr_atracacao dt_descarga_fim printd nr_movimento FROM zlest0202
       INTO TABLE t_xml
       WHERE dt_descarga_fim >= p_descfi
          AND dt_descarga_fim <= p_descff.

*  Busca Parâmetros
    SELECT * FROM zlest0201
      INTO TABLE t_zlest0201.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_montar_saida .

  DATA: w_zlest0061 LIKE LINE OF t_zlest0061,
        w_doc       LIKE LINE OF t_doc,
        w_saida     LIKE LINE OF t_saida,
        w_zlest0056 LIKE LINE OF t_zlest0056.

  DATA: l_nr_atracacao TYPE zlest0202-nr_atracacao.

  PERFORM zf_icon_create USING 'ICON_CHECKED ' TEXT-014 CHANGING w_icones-icon_complete.

  LOOP AT t_zlest0061 INTO w_zlest0061.

* O código da filial que possui código de “terminal de Uso Privado” é a “1002”,
    READ TABLE t_zlest0201 INTO w_zlest0201 WITH KEY  inst_portuaria  = w_constantes-inst_port_1002.

    READ TABLE t_zlest0056 INTO w_zlest0056 WITH KEY bukrs       = w_zlest0061-bukrs
                                                      werks      = w_zlest0061-werks
                                                      nr_viagem  = w_zlest0061-nr_viagem
                                                      ano_viagem = w_zlest0061-ano_viagem
                                                      po_destino+6(4) = w_zlest0201-inst_portuaria.

    CHECK sy-subrc IS INITIAL.

    MOVE-CORRESPONDING w_zlest0061 TO w_saida.

    READ TABLE t_doc INTO w_doc WITH KEY docnum = w_zlest0061-docnum.

    IF sy-subrc IS INITIAL.
      w_saida-nfenum = w_doc-nfenum.
    ENDIF.

    CLEAR w_zlest0053.
    READ TABLE t_zlest0053 INTO w_zlest0053 WITH KEY bukrs      = w_saida-bukrs
                                                     nome       = w_saida-nome_emb.
    IF sy-subrc IS INITIAL.
      w_saida-apelido = w_zlest0053-apelido.
      CONCATENATE w_saida-nr_viagem w_saida-ano_viagem+2(2) w_zlest0053-apelido(5) w_zlest0061-werks INTO l_nr_atracacao.
    ENDIF.

    CLEAR w_xml.
    READ TABLE t_xml INTO w_xml WITH KEY nr_atracacao = l_nr_atracacao.
    IF sy-subrc IS INITIAL.
      w_saida-nr_movimento = w_xml-nr_movimento.
      w_saida-nr_atracacao = w_xml-nr_atracacao.
      IF w_xml-printd IS NOT INITIAL.
        w_saida-status = w_icones-icon_complete.
      ENDIF.
    ENDIF.

    READ TABLE t_marc INTO DATA(w_marc) WITH KEY matnr = w_saida-cod_material.
    IF sy-subrc IS INITIAL.
      w_saida-codigo_carga              = w_marc-steuc(4).
    ENDIF.

    w_saida-hr_chegada          = w_zlest0061-hr_chegada.
    w_saida-dt_chegada          = w_zlest0061-dt_chegada.
    w_saida-dt_chegada_terminal	= w_zlest0061-dt_chegada_terminal.
    w_saida-dt_descarga_ini     = w_zlest0061-dt_descarga_ini.
    w_saida-hr_descarga_ini     = w_zlest0061-hr_descarga_ini.
    w_saida-dt_descarga_fim     = w_zlest0061-dt_descarga_fim.
    w_saida-hr_descarga_fim     = w_zlest0061-hr_descarga_fim.

    APPEND w_saida TO t_saida.

  ENDLOOP.

  IF t_saida[] IS INITIAL.
    APPEND INITIAL LINE TO t_saida.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_MONITOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_montar_monitor .

  IF t_saida[] IS NOT INITIAL.
    CALL SCREEN 9000.
    LEAVE LIST-PROCESSING.
  ELSE.
    MESSAGE i398(00) DISPLAY LIKE 'E' WITH 'Nenhum CT-e encontrado.'(004).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_status_9000 OUTPUT.

  SET PF-STATUS 'ZGMM_STATUS'.
  SET TITLEBAR  'ZUMM_TITULO'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_PREPARAR_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_preparar_alv OUTPUT.
  PERFORM zf_preparar_alv.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_alv .

  DATA: t_fcat TYPE          lvc_t_fcat,
        t_sort TYPE lvc_t_sort.

  DATA: w_variant TYPE disvariant,
        w_layout  TYPE lvc_s_layo,
        w_sort    LIKE LINE OF t_sort.

  DATA: dock     TYPE REF TO cl_gui_docking_container.

  w_layout-cwidth_opt = 'X'.
  w_layout-zebra      = 'X'.
  w_variant-report    = sy-repid.
*  W_VARIANT-VARIANT   = P_LAYOUT.

  CLEAR w_sort.
  w_sort-spos      = 1.
  w_sort-fieldname = 'DT_DESCARGA_FIM'.
  APPEND w_sort TO t_sort.

  SORT t_saida BY dt_descarga_fim.

  IF v_grid IS INITIAL.

    CREATE OBJECT dock
      EXPORTING
*       REPID                       = SY-REPID
        dynnr                       = '9000'
        extension                   = '1500'
*       side                        = cl_gui_docking_container=>dock_at_center
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc NE  0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT v_grid EXPORTING i_parent = dock.

    PERFORM zf_montar_fieldcat     CHANGING t_saida t_fcat.
    PERFORM zf_ajuste_descr_campos USING '9000'
                                   CHANGING t_fcat.

    PERFORM zf_definir_hotspot CHANGING t_fcat.

    CREATE OBJECT v_event_receiver.
    SET HANDLER v_event_receiver->handle_hotspot_click FOR v_grid.
    SET HANDLER v_event_receiver->handle_toolbar       FOR v_grid.
    SET HANDLER v_event_receiver->handle_user_command  FOR v_grid.

    w_layout-sel_mode = 'A'.

    CALL METHOD v_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = w_variant
        i_save                        = 'A'
        is_layout                     = w_layout
      CHANGING
        it_fieldcatalog               = t_fcat
        it_outtab                     = t_saida
        it_sort                       = t_sort
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   RAISE EVENT TOOLBAR TO SHOW THE MODIFIED TOOLBAR
    CALL METHOD v_grid->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_SAIDA  text
*      <--P_T_FIELDCAT  text
*----------------------------------------------------------------------*
FORM zf_montar_fieldcat  CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.
  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

* Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* Monta a estrutura dinâmica no objeto l_salv_table
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = abap_false
        IMPORTING
          r_salv_table = l_salv_table
        CHANGING
          t_table      = <f_table> ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
      RETURN.
  ENDTRY.

* Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

* Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CARREGA_LAYOUT_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_carrega_layout_alv .

  DATA: w_variant   TYPE disvariant.

  w_variant-report   = sy-repid.
  w_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = w_variant
      i_save     = 'A'
    IMPORTING
      es_variant = w_variant  "l_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc IS INITIAL.
*    P_LAYOUT = W_VARIANT-VARIANT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_adiciona_botoes_header  USING e_object TYPE REF TO cl_alv_event_toolbar_set.

* Add Button
  DATA: w_toolbar  TYPE stb_button.

* inclui novo item na barra do container
  CLEAR w_toolbar.
  MOVE 3 TO w_toolbar-butn_type.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
  PERFORM zf_icon_create USING 'ICON_TABLE_SETTINGS' TEXT-006 CHANGING w_icones-icon_table_settings.

  CLEAR w_toolbar.
  MOVE 'PARAMETROS'                     TO w_toolbar-function.
  MOVE w_icones-icon_table_settings TO w_toolbar-icon.
  MOVE '0 '                             TO w_toolbar-butn_type.
  MOVE 'Parâmetros'(007)                TO w_toolbar-quickinfo.
  MOVE 'Parâmetros'(007)                TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
  PERFORM zf_icon_create USING 'ICON_GENERATE' TEXT-005 CHANGING w_icones-icon_generate.

  CLEAR w_toolbar.
  MOVE 'GERARXML'                    TO w_toolbar-function.
  MOVE  w_icones-icon_generate       TO w_toolbar-icon.
  MOVE '0 '                          TO w_toolbar-butn_type.

  MOVE  v_disabled                   TO w_toolbar-disabled.
  MOVE 'Gerar XML'(006)              TO w_toolbar-quickinfo.
  MOVE 'Gerar XML'(006)              TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

  PERFORM zf_icon_create USING 'ICON_SYSTEM_PASTE' TEXT-006 CHANGING w_icones-icon_system_paste.

  CLEAR w_toolbar.
  MOVE 'GRAVARXML'                TO w_toolbar-function.
  MOVE w_icones-icon_system_paste  TO w_toolbar-icon.
  MOVE '0 '                       TO w_toolbar-butn_type.

  MOVE  v_disabled                   TO w_toolbar-disabled.
  MOVE 'Baixar Arquivo XML'(018)          TO w_toolbar-quickinfo.
  MOVE 'Baixar Arquivo XML'(018)          TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
      SET SCREEN 0.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  ZF_ICON_CREATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NOME_ICONE  text
*      -->P_TEXTO  text
*      -->P_ICONE  text
*----------------------------------------------------------------------*
FORM zf_icon_create  USING    p_nome_icone
                              p_texto
                              p_icone.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = p_nome_icone
      info   = p_texto
    IMPORTING
      result = p_icone.

ENDFORM.                    " ZF_ICON_CREATE
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_elimina_botoes_header  USING e_object TYPE REF TO cl_alv_event_toolbar_set.

*    elimina itens desnecessarios da barra do container
  DELETE e_object->mt_toolbar WHERE function = '&LOCAL&APPEND'
                                 OR function = '&LOCAL&INSERT_ROW'
                                 OR function = '&LOCAL&DELETE_ROW'
                                 OR function = '&LOCAL&COPY_ROW'
                                 OR function = '&LOCAL&CUT'
                                 OR function = '&LOCAL&COPY'
                                 OR function = '&LOCAL&PASTE'
                                 OR function = '&REFRESH'
                                 OR function = '&CHECK'
                                 OR function = '&GRAPH'
                                 OR function = '&INFO'
                                 OR function = '&LOCAL&UNDO'
                                 OR function = '&MB_VIEW'
*                                   OR function = '&MB_VARIANT'
                                 OR function = '&MB_EXPORT'
                                 OR function = '&PRINT_BACK'
                                 OR function = '&MB_SUM'
                                 OR function = '&MB_SUBTOT'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTE_DESCR_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_ajuste_descr_campos USING p_tela
                            CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING <fs_fcat>.

*----------------------
*  Descrição dos campos do ALV da Tela 9000
*----------------------
    IF p_tela = '9000'.
      CASE <fs_fcat>-fieldname.
        WHEN 'STATUS'.
          PERFORM zf_mostra_coluna USING 'STATUS'               'Status'(c01).
        WHEN 'NR_ATRACACAO'.
          PERFORM zf_mostra_coluna USING 'NR_ATRACACAO'         'Nro. Atracação'(c02).
        WHEN 'APELIDO'.
          PERFORM zf_mostra_coluna USING 'APELIDO'              'Apelido'(c27).
        WHEN 'DT_DESCARGA_FIM'.
          PERFORM zf_mostra_coluna USING 'DT_DESCARGA_FIM'      'Data Descarga Fim'(c03).
        WHEN 'HR_DESCARGA_FIM'.
          PERFORM zf_mostra_coluna USING 'HR_DESCARGA_FIM'      'Hora Descarga Fim'(c04).
        WHEN 'BUKRS'.
          PERFORM zf_mostra_coluna USING 'BUKRS'                'Empresa'(c05).
        WHEN 'WERKS'.
          PERFORM zf_mostra_coluna USING 'WERKS'                'Centro'(c06).
        WHEN 'ANO_VIAGEM'.
          PERFORM zf_mostra_coluna USING 'ANO_VIAGEM'           'Ano Viagem'(c07).
        WHEN 'NR_VIAGEM'.
          PERFORM zf_mostra_coluna USING 'NR_VIAGEM'            'Nro. Viagem'(c08).
        WHEN 'NOME_EMB'.
          PERFORM zf_mostra_coluna USING 'NOME_EMB'             'Nome Embarcação'(c09).
        WHEN 'COD_MATERIAL'.
          PERFORM zf_mostra_coluna USING 'COD_MATERIAL'         'Cód. Material'(c10).
        WHEN 'DT_FATURA'.
          PERFORM zf_mostra_coluna USING 'DT_FATURA'            'Data Fatura'(c11).
        WHEN 'PESO_VINCULADO'.
          PERFORM zf_mostra_coluna USING 'PESO_VINCULADO'       'Peso Vinculado'(c12).
        WHEN 'PESO_CHEGADA'.
          PERFORM zf_mostra_coluna USING 'PESO_CHEGADA'         'Peso Chegada'(c13).
        WHEN 'CL_CODIGO'.
          PERFORM zf_mostra_coluna USING 'CL_CODIGO'            'Cód. Cliente'(c14).
        WHEN 'SAFRA'.
          PERFORM zf_mostra_coluna USING 'SAFRA'                'Safra'(c15).
        WHEN 'DOCNUM'.
          PERFORM zf_mostra_coluna USING 'DOCNUM'               'Docnum'(c16).
        WHEN 'NFENUM'.
          PERFORM zf_mostra_coluna USING 'NFENUM'               'Nr. CT-e'(c17).
        WHEN 'DT_CHEGADA'.
          PERFORM zf_mostra_coluna USING 'DT_CHEGADA'           'Data Chegada'(c18).
        WHEN 'HR_CHEGADA'.
          PERFORM zf_mostra_coluna USING 'HR_CHEGADA'           'Hora Chegada'(c19).
        WHEN 'DT_CHEGADA_TERMINAL'.
          PERFORM zf_mostra_coluna USING 'DT_CHEGADA_TERMINAL'  'Data Chegada Terminal'(c20).
        WHEN 'DT_DESCARGA_INI'.
          PERFORM zf_mostra_coluna USING 'DT_DESCARGA_INI'      'Data Descarga Início'(c21).
        WHEN 'HR_DESCARGA_INI'.
          PERFORM zf_mostra_coluna USING 'HR_DESCARGA_INI'   'Hora Descarga Início'(c22).
        WHEN 'CODIGO_CARGA'.
          PERFORM zf_mostra_coluna USING 'CODIGO_CARGA'   'Código Carga(NCM)'(c28).
        WHEN OTHERS.
          <fs_fcat>-no_out = 'X'.
      ENDCASE.

    ENDIF.

*----------------------
*   Descrição dos campos do ALV da Tela 9001.
*----------------------
    IF p_tela = '9001'.
      CASE <fs_fcat>-fieldname.
        WHEN 'INST_PORTUARIA'.
          PERFORM zf_mostra_coluna USING 'INST_PORTUARIA'    'Inst. Port'(c23).
        WHEN 'TUP_INST_PORT'.
          PERFORM zf_mostra_coluna USING 'TUP_INST_PORT'     'Cód. TUP'(c24).
        WHEN 'LOCAL_ATRACACAO'.
          PERFORM zf_mostra_coluna USING 'LOCAL_ATRACACAO'   'Local Atracação'(c25).
        WHEN 'CNPJ_INST_PORT'.
          PERFORM zf_mostra_coluna USING 'CNPJ_INST_PORT'    'CNPJ'(c26).
        WHEN OTHERS.
          <fs_fcat>-no_out = 'X'.
      ENDCASE.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_MOSTRA_COLUNA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1017   text
*      -->P_1018   text
*----------------------------------------------------------------------*
FORM zf_mostra_coluna   USING p_fieldname p_desc.

  IF <fs_fcat>-fieldname = p_fieldname.
*    CLEAR: <FS_FCAT>-SCRTEXT_L, <FS_FCAT>-SCRTEXT_M, <FS_FCAT>-SCRTEXT_S.
**    <FS_FCAT>-REPTEXT = P_DESC.
*    <FS_FCAT>-TOOLTIP = <FS_FCAT>-COLTEXT = P_DESC.

    <fs_fcat>-scrtext_s = <fs_fcat>-scrtext_m = <fs_fcat>-scrtext_l = p_desc.
    <fs_fcat>-reptext = <fs_fcat>-coltext = <fs_fcat>-scrtext_l.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_status_9200 OUTPUT.
  SET PF-STATUS 'ZGMM_STATUS'.
  SET TITLEBAR  'ZUMM_TITULO_POPUP'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_PREPARAR_ALV_9001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_preparar_alv_9001 OUTPUT.
  PERFORM zf_selecionar_parametros.
  PERFORM zf_montar_saida_9001.
  PERFORM zf_preparar_alv_9001.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREPARAR_ALV_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preparar_alv_9001 .

  DATA: t_fcat      TYPE          lvc_t_fcat.

  DATA: w_variant TYPE disvariant,
        w_layout  TYPE lvc_s_layo.

  DATA: dock_9001     TYPE REF TO cl_gui_docking_container.

  w_layout-cwidth_opt = 'X'.
  w_layout-zebra      = 'X'.

  IF v_grid_9001 IS INITIAL.

    CREATE OBJECT dock_9001
      EXPORTING
*       REPID                       = SY-REPID
        dynnr                       = '9001'
        extension                   = '1500'
*       side                        = cl_gui_docking_container=>dock_at_center
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
    IF sy-subrc NE  0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CREATE OBJECT v_grid_9001 EXPORTING i_parent = dock_9001.

    PERFORM zf_montar_fieldcat CHANGING t_saida_9001 t_fcat.
    PERFORM zf_ajuste_descr_campos USING '9001'
                                   CHANGING t_fcat.

    CREATE OBJECT v_event_receiver_9001.
    SET HANDLER v_event_receiver_9001->handle_toolbar_9001       FOR v_grid_9001.
    SET HANDLER v_event_receiver_9001->handle_user_command_9001  FOR v_grid_9001.

    w_layout-sel_mode = 'A'.

    CALL METHOD v_grid_9001->set_table_for_first_display
      EXPORTING
        is_variant                    = w_variant
        i_save                        = 'A'
        is_layout                     = w_layout
      CHANGING
        it_fieldcatalog               = t_fcat
        it_outtab                     = t_saida_9001
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*   RAISE EVENT TOOLBAR TO SHOW THE MODIFIED TOOLBAR
    CALL METHOD v_grid_9001->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid_9001->refresh_table_display.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_BOTOES_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_elimina_botoes_9001
                   USING e_object TYPE REF TO cl_alv_event_toolbar_set.

  REFRESH: e_object->mt_toolbar.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ADICIONA_BOTOES_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*----------------------------------------------------------------------*
FORM zf_adiciona_botoes_9001
                USING e_object TYPE REF TO cl_alv_event_toolbar_set.

* Add Button
  DATA: w_toolbar  TYPE stb_button.

* inclui novo item na barra do container
  CLEAR w_toolbar.
  MOVE 3 TO w_toolbar-butn_type.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
  PERFORM zf_icon_create USING 'ICON_DISPLAY' TEXT-013 CHANGING w_icones-icon_display.

  CLEAR w_toolbar.
  MOVE 'DISPLAY'                TO w_toolbar-function.
  MOVE w_icones-icon_display     TO w_toolbar-icon.
  MOVE '0 '                     TO w_toolbar-butn_type.
  MOVE 'DISPLAY'(010)           TO w_toolbar-quickinfo.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
  PERFORM zf_icon_create USING 'ICON_CREATE' TEXT-005 CHANGING w_icones-icon_create.

  CLEAR w_toolbar.
  MOVE 'CREATE'                TO w_toolbar-function.
  MOVE w_icones-icon_create    TO w_toolbar-icon.
  MOVE '0 '                    TO w_toolbar-butn_type.
  MOVE 'CREATE'(010)           TO w_toolbar-quickinfo.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
  PERFORM zf_icon_create USING 'ICON_CHANGE' TEXT-005 CHANGING w_icones-icon_change.

  CLEAR w_toolbar.
  MOVE 'CHANGE'                TO w_toolbar-function.
  MOVE w_icones-icon_change    TO w_toolbar-icon.
  MOVE '0 '                    TO w_toolbar-butn_type.
  MOVE 'CHANGE'(011)           TO w_toolbar-quickinfo.
  APPEND w_toolbar TO e_object->mt_toolbar.

* inclui novo item na barra do container
  PERFORM zf_icon_create USING 'ICON_DELETE' TEXT-005 CHANGING w_icones-icon_delete.

  CLEAR w_toolbar.
  MOVE 'DELETE'                TO w_toolbar-function.
  MOVE w_icones-icon_delete    TO w_toolbar-icon.
  MOVE '0 '                    TO w_toolbar-butn_type.
  MOVE 'DELETE'(012)           TO w_toolbar-quickinfo.
  APPEND w_toolbar TO e_object->mt_toolbar.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_MONTAR_SAIDA_9001
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_montar_saida_9001 .

  IF t_saida_9001[] IS INITIAL.

    LOOP AT t_zlest0201 INTO w_zlest0201.
      MOVE-CORRESPONDING w_zlest0201 TO w_saida_9001.
      APPEND w_saida_9001 TO t_saida_9001.
    ENDLOOP.

  ENDIF.

  IF t_saida_9001[] IS INITIAL.
    APPEND INITIAL LINE TO t_saida_9001.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_status_9002 OUTPUT.
  SET PF-STATUS 'ZGMM_STATUS_9002'.
  SET TITLEBAR  'ZUMM_TITULO_POPUP'.

  LOOP AT SCREEN.

    IF screen-group4 = 'DIS'.
      screen-input = 0.
    ENDIF.

    IF screen-group2 = 'CHG' AND v_ucomm = 'CHANGE'.
      screen-input = 1.
    ENDIF.

    IF screen-group1 = 'CRE' AND v_ucomm = 'CREATE'.
      screen-input = 1.
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
*      SET SCREEN 0.
      SET SCREEN 0.
      LEAVE TO SCREEN 1000.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.
  SET PF-STATUS 'ZGMM_STATUS'.
  SET TITLEBAR  'ZUMM_TITULO'.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ZM_SELECIONA_TELA_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_seleciona_tela_get INPUT.
  IF w_tela-tela = 9000.
    w_tela-tela = 9001.
  ELSE.
    w_tela-tela = 9000.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_9001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_9001 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_9200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit_9200 INPUT.

  IF v_grid_9001 IS BOUND.
    CALL METHOD v_grid_9001->free.
  ENDIF.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT'.
*      SET SCREEN 9100.
*      LEAVE TO SCREEN 9100.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit_9002 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'CANC'.
*      SET SCREEN 9200.
*      CALL SCREEN 9200.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  W_PARAMETROS-INST_PORT_DESC  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE w_parametros-inst_port_desc INPUT.

* Preenche descrição do Local de Negócio
  PERFORM zf_busca_inst_port_desc.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_user_command_9002 INPUT.

  CASE sy-ucomm.
    WHEN 'SALVAR'.
      IF v_ucomm = 'CREATE'.
        PERFORM zf_salvar_parametros.
      ENDIF.
      IF v_ucomm = 'CHANGE'.
        PERFORM zf_salvar_parametros.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_SALVAR_PARAMETROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_salvar_parametros .

  DATA: w_parametros_duprec TYPE zlest0201.

*Verifica-se já não existe entrada.
  CLEAR: w_parametros_duprec.
  SELECT SINGLE * FROM zlest0201
    INTO w_parametros_duprec
    WHERE inst_portuaria = w_parametros-inst_portuaria
    AND tup_inst_port    = w_parametros-tup_inst_port
    AND cnpj_inst_port   = w_parametros-cnpj_inst_port
    AND local_atracacao  = w_parametros-local_atracacao.

  IF sy-subrc IS INITIAL AND v_ucomm = 'CREATE'.
    MESSAGE s000 WITH 'Parâmetros já cadastrados!'(t01)
                 DISPLAY LIKE 'E'.
  ELSE.

    CLEAR: w_zlest0201.   REFRESH t_saida_9001.

* Dados criação
    IF v_ucomm = 'CREATE'.
      MOVE:
      sy-uname   TO w_parametros-usuario_criacao,
      sy-datum   TO w_parametros-data_criacao,
      sy-uzeit   TO w_parametros-hr_criacao.
    ENDIF.

*  Dados Alteração
    IF v_ucomm = 'CHANGE'.
      MOVE:
      sy-uname   TO w_parametros-us_modif,
      sy-datum   TO w_parametros-dt_modif,
      sy-uzeit   TO w_parametros-hr_modif.
    ENDIF.

    MOVE-CORRESPONDING w_parametros TO w_zlest0201.
    MODIFY zlest0201 FROM w_zlest0201.

    IF sy-subrc IS INITIAL.
      COMMIT WORK.

      MESSAGE s000 WITH 'Dados salvos com sucesso!'(t02).
      LEAVE TO SCREEN 9001."9200.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SELECIONA_PARAMETROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_selecionar_parametros .

* Busca Parâmetros Cadastrados
  REFRESH: t_zlest0201.
  SELECT * FROM zlest0201
    INTO TABLE t_zlest0201.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_BUSCA_INST_PORT_DESC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_busca_inst_port_desc .

  CLEAR w_parametros-inst_port_desc.

  SELECT SINGLE name1 FROM t001w
    INTO w_parametros-inst_port_desc
    WHERE werks = w_parametros-inst_portuaria.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ELIMINA_REGISTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_elimina_registro .

  DELETE FROM zlest0201
           WHERE inst_portuaria  = w_parametros-inst_portuaria
             AND tup_inst_port   = w_parametros-tup_inst_port
             AND cnpj_inst_port  = w_parametros-cnpj_inst_port
             AND local_atracacao = w_parametros-local_atracacao.

  IF sy-subrc IS INITIAL.
    COMMIT WORK.

    MESSAGE s000 WITH 'Dados eliminados com sucesso!'(t05).

    REFRESH: t_saida_9001.
    PERFORM zf_selecionar_parametros.
    PERFORM zf_montar_saida_9001.
    CALL METHOD v_grid_9001->refresh_table_display.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GERAR_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_gerar_xml .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        l_linhas     TYPE sy-tabix,
        l_string     TYPE string.

  DATA: w_row_no LIKE LINE OF t_row_no.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000 WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000 WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_saida INTO w_saida INDEX w_row_no-row_id.

    IF sy-subrc = 0.

*      IF W_SAIDA-NR_ATRACACAO IS INITIAL.

      PERFORM zf_preencher_campos_xml.
      PERFORM  zf_transformar_tab_xml.
      PERFORM zf_baixar_xml USING w_saida.

*      ELSE.
*
** Verifica se documento já foi enviado
*        CLEAR: V_NR_MOVIM, V_PRINTD.
*        SELECT SINGLE PRINTD NR_MOVIMENTO FROM ZLEST0202
*          INTO (V_PRINTD, V_NR_MOVIM)
*          WHERE NR_ATRACACAO = W_SAIDA-NR_ATRACACAO.
*
*        IF SY-SUBRC IS INITIAL.
*
** Busca XML Gerado e Armazenado
*          CLEAR V_XML.
*          SELECT SINGLE XML FROM ZLEST0203
*            INTO (L_STRING)
*            WHERE NR_MOVIMENTO = V_NR_MOVIM.
*
*          IF SY-SUBRC IS INITIAL.
*            V_XML = L_STRING.
*            PERFORM  ZF_TRANSFORMAR_TAB_XML.
*            PERFORM ZF_BAIXAR_XML USING W_SAIDA.
*
**            PERFORM ZF_BAIXAR_XML_REIMPRIMIR.
*            CLEAR: V_PRINTD, V_XML.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.

    ENDIF.

  ENDLOOP.

  CALL METHOD v_grid->refresh_table_display.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_GRAVAR_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_gravar_xml .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        l_linhas     TYPE sy-tabix,
        l_string     TYPE string.

  DATA: w_row_no LIKE LINE OF t_row_no.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000 WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000 WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    READ TABLE t_saida INTO w_saida INDEX w_row_no-row_id.

    IF sy-subrc = 0.

      IF w_saida-nr_atracacao IS INITIAL.

*      PERFORM ZF_PREENCHER_CAMPOS_XML.
*      PERFORM  ZF_TRANSFORMAR_TAB_XML.
*      PERFORM ZF_BAIXAR_XML USING W_SAIDA.
        MESSAGE i000 WITH 'Arquivo XML não encontrado'(t04) DISPLAY LIKE 'E'.
        RETURN.

      ELSE.

* Verifica se documento já foi enviado
        CLEAR: v_nr_movim, v_printd.
        SELECT SINGLE printd nr_movimento FROM zlest0202
          INTO (v_printd, v_nr_movim)
          WHERE nr_atracacao = w_saida-nr_atracacao.

        IF sy-subrc IS INITIAL.

* Busca XML Gerado e Armazenado
          CLEAR v_xml.
          SELECT SINGLE xml FROM zlest0203
            INTO (l_string)
            WHERE nr_movimento = v_nr_movim.

          IF sy-subrc IS INITIAL.
            v_xml = l_string.
            PERFORM  zf_transformar_tab_xml.
            PERFORM zf_baixar_xml USING w_saida.

*            PERFORM ZF_BAIXAR_XML_REIMPRIMIR.
            CLEAR: v_printd, v_xml.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDLOOP.

  CALL METHOD v_grid->refresh_table_display.
*  DATA: T_INDEX_ROWS TYPE LVC_T_ROW,                        "#EC NEEDED
*        T_ROW_NO     TYPE LVC_T_ROID,
*        W_ROW_NO     LIKE LINE OF T_ROW_NO,
*        W_SAIDA      LIKE LINE OF T_SAIDA,
*        L_LINHAS     TYPE SY-TABIX.
*
*  CALL METHOD V_GRID->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = T_INDEX_ROWS
*      ET_ROW_NO     = T_ROW_NO.
*
*  DESCRIBE TABLE T_ROW_NO LINES L_LINHAS.
*  IF L_LINHAS = 0.
*    "Selecionar uma linha.
*    MESSAGE I000 WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
*    RETURN.
*  ELSEIF L_LINHAS > 1.
*    "Selecionar apenas uma linha
*    MESSAGE I000 WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*
*  LOOP AT T_ROW_NO INTO W_ROW_NO.
*    READ TABLE T_SAIDA INTO W_SAIDA INDEX W_ROW_NO-ROW_ID.
*    IF SY-SUBRC = 0.
*      PERFORM ZF_PREENCHER_CAMPOS_XML.
*    ENDIF.
*  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VISUALIZAR_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_visualizar_xml .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        w_row_no     LIKE LINE OF t_row_no,
        w_saida      LIKE LINE OF t_saida,
        l_linhas     TYPE sy-tabix.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000 WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000 WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.
    READ TABLE t_saida INTO w_saida INDEX w_row_no-row_id.
    IF sy-subrc = 0.
      PERFORM zf_call_transaction_zles0186 USING w_saida.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_PREENCHER_CAMPOS_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_preencher_campos_xml .

  DATA: t_saida_aux TYPE TABLE OF y_saida.

  DATA: w_atracacao LIKE LINE OF w_abap-atracacao, "ZLESMESSAGE1_ATRACACAO,
        w_carga     LIKE LINE OF t_carga,
        w_marc      LIKE LINE OF t_marc,
        w_zlest0056 LIKE LINE OF t_zlest0056.

  DATA: l_data_hora_chegada            TYPE c LENGTH 25,
        l_data_hora_prevista_atracacao TYPE c LENGTH 25,
        l_data_hora_atracacao          TYPE c LENGTH 25,
        l_data_hora_desatracacao       TYPE c LENGTH 25,
        l_data_hora_inicio_operacao    TYPE c LENGTH 25,
        l_data_hora_termino_operacao   TYPE c LENGTH 25,
        l_tabix                        TYPE sy-tabix,
        l_datatype                     TYPE prx_r3name,
        l_capitania                    TYPE c LENGTH 15,
        l_hr_prevista                  TYPE zlest0061-hr_chegada,
        l_dt_prevista                  TYPE zlest0061-dt_chegada,
        l_hr_atracacao                 TYPE zlest0061-hr_chegada,
        l_dt_atracacao                 TYPE zlest0061-dt_chegada,
        l_hr_desatracacao              TYPE zlest0061-hr_chegada,
        l_dt_desatracacao              TYPE zlest0061-dt_chegada,
        l_carac_especial               TYPE string.

  l_carac_especial  = '‘"!@#$%¨&*()_+=-`´`[]~^/?;:.,><’'.

  l_datatype                     = 'ZLESMESSAGE1'.

  SORT t_zlest0061 BY ano_viagem nr_viagem nome_emb.

* Considera CT-es apenas do mês selecionado
  CLEAR w_abap. UNASSIGN <fs_saida>. REFRESH: t_saida_aux.
  LOOP AT t_saida ASSIGNING <fs_saida> WHERE dt_descarga_fim+4(2) = w_saida-dt_descarga_fim+4(2).

* preenche número do movimento
    CONCATENATE w_constantes-inst_port_1002
                <fs_saida>-dt_descarga_fim+4(2)
                <fs_saida>-dt_descarga_fim(4) INTO <fs_saida>-nr_movimento.

    READ TABLE t_zlest0053 INTO w_zlest0053 WITH KEY bukrs      = <fs_saida>-bukrs
                                                     nome       = <fs_saida>-nome_emb.
    IF sy-subrc IS INITIAL.
      CONCATENATE <fs_saida>-nr_viagem <fs_saida>-ano_viagem+2(2) w_zlest0053-apelido(5) <fs_saida>-werks INTO <fs_saida>-nr_atracacao.
      APPEND <fs_saida> TO t_saida_aux.
    ENDIF.

  ENDLOOP.

  SORT t_saida     BY nr_atracacao.
  SORT t_saida_aux BY nr_atracacao.
  DELETE ADJACENT DUPLICATES FROM t_saida_aux COMPARING nr_atracacao.

  CLEAR w_abap. UNASSIGN <fs_saida>.
  LOOP AT t_saida_aux ASSIGNING <fs_saida>.

    CLEAR: w_atracacao, w_carga, w_marc.
    REFRESH: t_carga.

* O código da filial que possui código de “terminal de Uso Privado” é a “1002”,
    CLEAR w_zlest0201.
    READ TABLE t_zlest0201 INTO w_zlest0201 WITH KEY  inst_portuaria  = w_constantes-inst_port_1002.

    CLEAR w_zlest0053.
    READ TABLE t_zlest0053 INTO w_zlest0053 WITH KEY bukrs      = <fs_saida>-bukrs
                                                     nome       = <fs_saida>-nome_emb.

* preenche informações da atracação
    w_atracacao-codigo_tup                     = w_zlest0201-tup_inst_port.

    w_atracacao-numero_atracacao = <fs_saida>-nr_atracacao.

    w_atracacao-local_atracacao                = w_zlest0201-local_atracacao.
    w_atracacao-imon                           = w_constantes-imon_0.
    l_capitania                                = w_zlest0053-insc_marinha."( inscrição da  Marinha  mercante – pegar  somente  os  números)

    REPLACE ALL OCCURRENCES OF '-' IN l_capitania WITH space.
    REPLACE ALL OCCURRENCES OF '.' IN l_capitania WITH space.
    REPLACE ALL OCCURRENCES OF '/' IN l_capitania WITH space.
    CONDENSE l_capitania.

    w_atracacao-numero_inscricao_capitania = l_capitania.
    w_atracacao-navegacao                      = w_constantes-navegacao_1.
    w_atracacao-tipo_operacao                  = w_constantes-tipo_operacao_1.
    w_atracacao-cnpj_agente                    = w_zlest0201-cnpj_ag_maritimo.
    w_atracacao-nacionalidade_armador          = w_zlest0201-nacional_armador.
    w_atracacao-cnpj_codigo_armador            = w_zlest0201-cnpj_armador.

* formatação de data - yyyy-MM-ddThh:mm:ss
*----------------------------------------------------------------------*
*DataHoraChegada – Data e hora da chegada da embarcação na instalação
*autorizada
*----------------------------------------------------------------------*
    PERFORM zf_ajusta_data_hora USING <fs_saida>-dt_chegada
                                      <fs_saida>-hr_chegada
                                CHANGING l_data_hora_chegada.

*----------------------------------------------------------------------*
*DataHoraPrevistaAtracacao – Data e hora prevista para a embarcação
* atracar na instalação autorizada. Subtraído 10 minutos da chegada
*----------------------------------------------------------------------*
    CLEAR: l_hr_prevista, l_dt_prevista.
    PERFORM zf_ajustar_hora  USING  <fs_saida>-dt_descarga_ini
                                    <fs_saida>-hr_descarga_ini
                                    w_constantes-10minutos
                                    w_constantes-subtrair
                                    CHANGING
                                    l_dt_prevista
                                    l_hr_prevista.

    PERFORM zf_ajusta_data_hora USING l_dt_prevista
                                      l_hr_prevista
                                CHANGING l_data_hora_prevista_atracacao.

*----------------------------------------------------------------------*
*DataHoraAtracacao – Data e hora da atracação da embarcação na instalação
*autorizada. Subtraido 5 minutos do Inicio da Operação
*----------------------------------------------------------------------*
    CLEAR: l_hr_atracacao,  l_dt_atracacao.
    PERFORM zf_ajustar_hora  USING  <fs_saida>-dt_descarga_ini
                                    <fs_saida>-hr_descarga_ini
                                    w_constantes-5minutos
                                    w_constantes-subtrair
                                    CHANGING
                                    l_dt_atracacao
                                    l_hr_atracacao.


    PERFORM zf_ajusta_data_hora USING l_dt_atracacao
                                      l_hr_atracacao
                                CHANGING l_data_hora_atracacao.

*----------------------------------------------------------------------*
*DataHoraInicioOperacao - Data e hora do inicio da Operação
*----------------------------------------------------------------------*
    PERFORM zf_ajusta_data_hora USING <fs_saida>-dt_descarga_ini
                                      <fs_saida>-hr_descarga_ini
                           CHANGING  l_data_hora_inicio_operacao.

*----------------------------------------------------------------------*
*DataHoraTerminoOperacao - Data e hora do término da operação
*----------------------------------------------------------------------*
    PERFORM zf_ajusta_data_hora USING <fs_saida>-dt_descarga_fim
                                      <fs_saida>-hr_descarga_fim
                    CHANGING l_data_hora_termino_operacao.

*----------------------------------------------------------------------*
*DataHoraDesatracacao – Data e hora da desatracação da embarcação na
*instalação. Adicionado 5 minutos ao final da Operação
*----------------------------------------------------------------------*
    CLEAR: l_hr_desatracacao,  l_dt_desatracacao.
    PERFORM zf_ajustar_hora  USING  <fs_saida>-dt_descarga_fim
                                    <fs_saida>-hr_descarga_fim
                                    w_constantes-10minutos
                                    w_constantes-adicionar
                                    CHANGING
                                    l_dt_desatracacao
                                    l_hr_desatracacao.


    PERFORM zf_ajusta_data_hora USING l_dt_desatracacao
                                      l_hr_desatracacao
                            CHANGING l_data_hora_desatracacao.


    w_atracacao-data_hora_chegada            = l_data_hora_chegada.
    w_atracacao-data_hora_prevista_atracacao = l_data_hora_prevista_atracacao.
    w_atracacao-data_hora_atracacao          = l_data_hora_atracacao.
    w_atracacao-data_hora_desatracacao       = l_data_hora_desatracacao.
    w_atracacao-data_hora_inicio_operacao    = l_data_hora_inicio_operacao.
    w_atracacao-data_hora_termino_operacao   = l_data_hora_termino_operacao.
    w_atracacao-flag_possui_coleta_residuo   = '0'.

    CLEAR: w_carga, w_saida. REFRESH: t_carga.
    READ TABLE t_saida INTO w_saida WITH KEY nr_atracacao =  <fs_saida>-nr_atracacao
                                                                    BINARY SEARCH.

    IF sy-subrc IS INITIAL.

      CLEAR l_tabix.
      l_tabix = sy-tabix.

      DO.

* preenche informações sobre a carga
        w_carga-cnpj_operador = w_zlest0201-cnpj_operador.

        READ TABLE t_marc INTO w_marc WITH KEY matnr = <fs_saida>-cod_material.
        IF sy-subrc IS INITIAL.
          w_carga-codigo_carga              = w_marc-steuc(4).
        ENDIF.

        w_carga-natureza_carga            = w_constantes-natureza_carga_1.
        w_carga-sentido                   = w_constantes-sentido_1.
        w_carga-imdg_code = '1'.

        READ TABLE t_zlest0056 INTO w_zlest0056 WITH KEY bukrs       = w_saida-bukrs
                                                          werks      = w_saida-werks
                                                          nr_viagem  = w_saida-nr_viagem
                                                          ano_viagem = w_saida-ano_viagem
                                                          po_destino+6(4) = w_zlest0201-inst_portuaria.

        IF w_zlest0056-po_embarque+6(4) = w_constantes-po_embarque_1003.
          w_carga-trigrama_porto_origem = w_constantes-big_pais_orig_pvh.
          w_carga-bigrama_pais_origem   = w_constantes-trig_porto_orig_br.
        ENDIF.

        IF w_zlest0056-po_embarque+6(4) = w_constantes-po_embarque_0161.
          w_carga-codigo_tuporigem      = w_constantes-cod_tupori_brro013.
        ENDIF.

        IF w_zlest0056-po_embarque+6(4) = w_constantes-po_embarque_1001.
          w_carga-codigo_tuporigem      = w_constantes-cod_tupori_bram029.
        ENDIF.

        IF w_zlest0056-po_embarque      = w_constantes-po_embarque_0000229649.
          w_carga-codigo_tuporigem      = w_constantes-cod_tupori_bram079.
        ENDIF.

*-IR209574-19.11.2024-#158940-JT-inicio
        IF w_zlest0056-po_embarque+6(4) = w_constantes-po_embarque_3904.
          w_carga-codigo_tuporigem      = w_constantes-cod_tupori_brpa038.
        ENDIF.

        IF w_zlest0056-po_embarque      = w_constantes-po_embarque_0000188702.
          w_carga-trigrama_porto_origem = w_constantes-big_pais_orig_stm.
          w_carga-bigrama_pais_origem   = w_constantes-trig_porto_orig_br.
        ENDIF.

        IF w_zlest0056-po_embarque      = w_constantes-po_embarque_0000538943.
          w_carga-trigrama_porto_origem = w_constantes-big_pais_orig_stm.
          w_carga-bigrama_pais_origem   = w_constantes-trig_porto_orig_br.
        ENDIF.
*-IR209574-19.11.2024-#158940-JT-fim

        IF w_zlest0056-po_destino+6(4)  = w_constantes-po_destino_1002.
          w_carga-codigo_tupdestino     = w_constantes-po_tupdest_bram014.
          w_carga-codigo_area_destino   = w_constantes-cod_area_dest_a1.
        ENDIF.

*-IR224070-19.02.2025-#167410-JT-inicio
        IF w_zlest0056-po_embarque      = w_constantes-po_embarque_0000186993.
          w_carga-codigo_tuporigem      = w_constantes-cod_tupori_bram091.
        ENDIF.
*-IR224070-19.02.2025-#167410-JT-fim

*  w_carga-quantidade                  = "Não utilizado
        w_carga-peso_carga_bruta            = w_carga-peso_carga_bruta + w_saida-peso_chegada.
        w_carga-peso_carga_bruta_propria    = w_carga-peso_carga_bruta_propria + w_saida-peso_chegada.

        IF w_zlest0056-po_destino+6(4)      = w_constantes-po_destino_1002
          AND w_carga-codigo_tupdestino     = w_constantes-po_tupdest_bram014
          AND w_carga-codigo_area_destino   = w_constantes-cod_area_dest_a1.

        ELSE.
          w_carga-peso_carga_bruta_terceiros  = w_carga-peso_carga_bruta_terceiros + w_saida-peso_chegada.
        ENDIF.

* Campo não deve ser informado para tipo de Navegação 1
        w_carga-navegacao                   = w_constantes-navegacao_1.
        w_carga-tipo_operacao_carga         = w_constantes-cod_op_carga_18.

        ADD 1 TO l_tabix.

        READ TABLE t_saida INTO w_saida INDEX l_tabix.
        IF sy-subrc <> 0 OR <fs_saida>-nr_atracacao <>  w_saida-nr_atracacao.

          w_carga-peso_carga_bruta            = w_carga-peso_carga_bruta / w_constantes-peso_1000.
          w_carga-peso_carga_bruta_propria    = w_carga-peso_carga_bruta_propria / w_constantes-peso_1000.
          w_carga-peso_carga_bruta_terceiros  = w_carga-peso_carga_bruta_terceiros / w_constantes-peso_1000.

          APPEND w_carga TO t_carga.
          w_atracacao-carga[] = t_carga[].
          EXIT.

        ENDIF.

      ENDDO.

      APPEND  w_atracacao TO w_abap-atracacao[].

    ENDIF.

  ENDLOOP.

  REFRESH: t_saida_aux[].
  SORT t_saida BY ano_viagem nr_viagem nome_emb.

  TRY.

* cria XML
      v_xml = cl_proxy_xml_transform=>abap_to_xml_xstring(
        abap_data  = w_abap
        ddic_type  = l_datatype
*       ext_xml    = ''
        xml_header = 'full'
      ).

    CATCH cx_transformation_error .
    CATCH cx_root .                                      "#EC CATCH_ALL

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTA_DATA_HORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
* (yyyy-MM-ddThh:mm:ss)
*----------------------------------------------------------------------*
FORM zf_ajusta_data_hora  USING    p_dt_in
                                   p_hr_in
                          CHANGING p_dt_hr.

  DATA: l_dt_in TYPE c LENGTH 8,
        l_hr_in TYPE c LENGTH 8.

* ajuste de data
  IF p_dt_in IS NOT INITIAL.
    l_dt_in = p_dt_in.
  ELSE.
    l_dt_in = '00010101'.
  ENDIF.

  CONCATENATE l_dt_in(4) l_dt_in+4(2) l_dt_in+6(2) INTO p_dt_hr(10) SEPARATED BY '-'.

  CONCATENATE p_dt_hr 'T' INTO p_dt_hr.

* ajuste de hora
  IF p_hr_in IS NOT INITIAL.
    l_hr_in = p_hr_in.
  ELSE.
    l_hr_in = '000000'.
  ENDIF.

  CONCATENATE l_hr_in(2) l_hr_in+2(2) l_hr_in+4(3) INTO p_dt_hr+11(10) SEPARATED BY ':'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_HANDLE_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID_INDEX  text
*      -->P_E_COLUMN_ID  text
*----------------------------------------------------------------------*
FORM zf_handle_hotspot_click   USING    p_index  TYPE any
                                       p_column TYPE any.

  DATA: w_saida LIKE LINE OF t_saida.

  READ TABLE t_saida INTO w_saida INDEX p_index.

  CASE p_column.
    WHEN 'NR_ATRACACAO'.
      PERFORM zf_call_transaction_zles0186 USING w_saida.
    WHEN 'DOCNUM'.
    WHEN OTHERS.
      PERFORM zf_call_transaction_j1b3n USING w_saida.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_DEFINIR_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_T_FIELDCAT  text
*----------------------------------------------------------------------*
FORM zf_definir_hotspot   CHANGING pt_fieldcat TYPE lvc_t_fcat.

  FIELD-SYMBOLS: <fs_fcat> LIKE LINE OF pt_fieldcat.

  LOOP AT pt_fieldcat ASSIGNING <fs_fcat>.
    IF <fs_fcat>-fieldname = 'NR_ATRACACAO'.
      <fs_fcat>-hotspot = 'X'.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_ZLES0186
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_call_transaction_zles0186  USING p_saida TYPE y_saida.

  IF p_saida-nr_atracacao IS NOT INITIAL.
    SET PARAMETER ID 'ATR' FIELD p_saida-nr_atracacao.
    CALL TRANSACTION 'ZLES0186' AND SKIP FIRST SCREEN. "USING T_BDCDATA MODE 'E'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_BDC_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0586   text
*      -->P_0587   text
*----------------------------------------------------------------------*
FORM zf_bdc_dynpro   USING p_prog
                         p_scr.

  DATA: w_bdcdata LIKE LINE OF t_bdcdata.

  w_bdcdata-program  = p_prog.
  w_bdcdata-dynpro   = p_scr.
  w_bdcdata-dynbegin = abap_true.

  APPEND w_bdcdata TO t_bdcdata.
  CLEAR w_bdcdata.

ENDFORM.                    " ZF_BDC_DYNPRO
*&---------------------------------------------------------------------*
*&      Form  ZF_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0591   text
*      -->P_0592   text
*----------------------------------------------------------------------*
FORM zf_bdc_field USING p_fnam
                         p_fval.

  DATA: w_bdcdata LIKE LINE OF t_bdcdata.

  w_bdcdata-fnam   = p_fnam.
  w_bdcdata-fval   = p_fval.

  APPEND w_bdcdata TO t_bdcdata.
  CLEAR w_bdcdata.

ENDFORM.                    " ZF_BDC_FIELD
*&---------------------------------------------------------------------*
*&      Form  ZF_BAIXAR_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_baixar_xml USING pw_saida TYPE y_saida.

  TYPES: BEGIN OF y_arq_xml,
           string TYPE string,
         END OF y_arq_xml.

  DATA: t_zlest0202 TYPE TABLE OF zlest0202,
        w_zlest0202 LIKE LINE OF t_zlest0202.

  DATA: t_zlest0203 TYPE TABLE OF zlest0203,
        w_zlest0203 LIKE LINE OF t_zlest0203.


  DATA: t_arq_xml TYPE TABLE OF y_arq_xml,
        w_arq_xml LIKE LINE OF t_arq_xml,
        l_tamanho TYPE i.

  DATA : l_fullpath    TYPE                   string,
         l_filename    TYPE                   string,
         l_path        TYPE                   string,
         l_user_action TYPE                   i,
         l_encoding    TYPE                   abap_encoding.


  IF l_filename IS INITIAL.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title         = 'Caminho para salvar arquivos XML'
        with_encoding        = 'X'
        initial_directory    = 'C:\TEMP'
      CHANGING
        filename             = l_filename
        path                 = l_path
        fullpath             = l_fullpath
        user_action          = l_user_action
        file_encoding        = l_encoding
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

  ENDIF.

  IF l_filename IS NOT INITIAL.

    MOVE v_string TO w_arq_xml-string.
    APPEND w_arq_xml TO t_arq_xml.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = l_filename
        filetype                = 'ASC'
      CHANGING
        data_tab                = t_arq_xml[]
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

* Erro ao Gravar Arquivo - Mantém campo PRINTD vazio
    IF sy-subrc <> 0.
      CLEAR l_filename.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      v_printd = abap_true.
    ENDIF.

  ENDIF.

  UNASSIGN <fs_saida>.
  LOOP AT t_saida ASSIGNING <fs_saida>  WHERE dt_descarga_fim+4(2)  = pw_saida-dt_descarga_fim+4(2).

    IF v_printd = abap_true.
      <fs_saida>-status           = w_icones-icon_complete.
    ENDIF.

    CHECK <fs_saida>-nr_atracacao IS NOT INITIAL.

    w_zlest0202-nr_movimento    = <fs_saida>-nr_movimento.
    w_zlest0202-nr_atracacao    = <fs_saida>-nr_atracacao.
    w_zlest0202-dt_descarga_fim = <fs_saida>-dt_descarga_fim.
    w_zlest0202-printd          = v_printd.
    APPEND w_zlest0202 TO t_zlest0202.

* GRAVA XML DO MOVIMENTO
    IF v_nr_movim IS INITIAL.
      w_zlest0203-nr_movimento  = w_zlest0202-nr_movimento.
      w_zlest0203-xml           = v_xml.
      w_zlest0203-dt_movimento  = sy-datum.
      w_zlest0203-usnam         = sy-uname.
      APPEND w_zlest0203 TO t_zlest0203.

*    v_nr_movim  = w_zlest0202-nr_movimento. ""*-IR209574-19.11.2024-#158940-JT-inicio
    ENDIF.

  ENDLOOP.

  IF t_zlest0202[] IS NOT INITIAL.
    MODIFY zlest0202 FROM TABLE t_zlest0202.
    COMMIT WORK.
  ENDIF.

  IF t_zlest0203[] IS NOT INITIAL.
    MODIFY zlest0203 FROM TABLE t_zlest0203.
    COMMIT WORK.
  ENDIF.

  CLEAR: v_xml. REFRESH: t_zlest0202.

  CALL METHOD v_grid->refresh_table_display.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_BAIXAR_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_baixar_xml_reimprimir.

  TYPES: BEGIN OF y_arq_xml,
           string TYPE string,
         END OF y_arq_xml.

  DATA: t_zlest0202 TYPE TABLE OF zlest0202,
        w_zlest0202 LIKE LINE OF t_zlest0202.

  DATA: t_zlest0203 TYPE TABLE OF zlest0203,
        w_zlest0203 LIKE LINE OF t_zlest0203.


  DATA: t_arq_xml TYPE TABLE OF y_arq_xml,
        w_arq_xml LIKE LINE OF t_arq_xml,
        l_tamanho TYPE i.

  DATA : l_fullpath    TYPE                   string,
         l_filename    TYPE                   string,
         l_path        TYPE                   string,
         l_user_action TYPE                   i,
         l_encoding    TYPE                   abap_encoding.


  IF l_filename IS INITIAL.

    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        window_title         = 'Caminho para salvar arquivos XML'
        with_encoding        = 'X'
        initial_directory    = 'C:\TEMP'
      CHANGING
        filename             = l_filename
        path                 = l_path
        fullpath             = l_fullpath
        user_action          = l_user_action
        file_encoding        = l_encoding
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        OTHERS               = 4.

  ENDIF.

  IF l_filename IS NOT INITIAL.

    MOVE v_string TO w_arq_xml-string.
    APPEND w_arq_xml TO t_arq_xml.

    CALL METHOD cl_gui_frontend_services=>gui_download
      EXPORTING
        filename                = l_filename
        filetype                = 'ASC'
      CHANGING
        data_tab                = t_arq_xml[]
      EXCEPTIONS
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        not_supported_by_gui    = 22
        error_no_gui            = 23
        OTHERS                  = 24.

* Erro ao Gravar Arquivo - Mantém campo PRINTD vazio
    IF sy-subrc <> 0.

      UNASSIGN <fs_saida>.
      LOOP AT t_saida ASSIGNING <fs_saida>.

        w_zlest0202-nr_movimento    = <fs_saida>-nr_movimento.
        w_zlest0202-nr_atracacao    = <fs_saida>-nr_atracacao.
        w_zlest0202-dt_descarga_fim = <fs_saida>-dt_descarga_fim.
        w_zlest0202-printd          = v_printd.
        APPEND w_zlest0202 TO t_zlest0202.

* GRAVA XML DO MOVIMENTO
        AT NEW nr_movimento.
          w_zlest0203-nr_movimento  = w_zlest0202-nr_movimento.
          w_zlest0203-xml           = v_xml.
          w_zlest0203-dt_movimento  = sy-datum.
          w_zlest0203-usnam         = sy-uname.
          APPEND w_zlest0203 TO t_zlest0203.
        ENDAT.

      ENDLOOP.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

      IF v_nr_movim IS INITIAL.

        LOOP AT t_saida ASSIGNING <fs_saida>.

          <fs_saida>-status           = w_icones-icon_complete.
          w_zlest0202-nr_movimento    = <fs_saida>-nr_movimento.
          w_zlest0202-nr_atracacao    = <fs_saida>-nr_atracacao.
          w_zlest0202-printd          = abap_true.
          w_zlest0202-dt_descarga_fim = <fs_saida>-dt_descarga_fim.

          APPEND w_zlest0202 TO t_zlest0202.

          AT NEW nr_movimento.

            IF v_nr_movim IS INITIAL.
              w_zlest0203-nr_movimento    = w_zlest0202-nr_movimento.
              w_zlest0203-xml             = v_xml.
              w_zlest0203-dt_movimento    = sy-datum.
              w_zlest0203-usnam           = sy-uname.
              APPEND w_zlest0203 TO t_zlest0203.
            ENDIF.

          ENDAT.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ELSE.

* ATUALIZA CONTROLE DE ENVIO
    UNASSIGN <fs_saida>.
    LOOP AT t_saida ASSIGNING <fs_saida>.

      CHECK <fs_saida>-nr_atracacao IS NOT INITIAL.

      w_zlest0202-nr_movimento    = <fs_saida>-nr_movimento.
      w_zlest0202-nr_atracacao    = <fs_saida>-nr_atracacao.
      w_zlest0202-dt_descarga_fim = <fs_saida>-dt_descarga_fim.
      w_zlest0202-printd          = v_printd.
      APPEND w_zlest0202 TO t_zlest0202.

* Grava XML do Movimento
      IF v_nr_movim IS INITIAL.
        w_zlest0203-nr_movimento  = w_zlest0202-nr_movimento.
        w_zlest0203-xml           = v_xml.
        w_zlest0203-dt_movimento  = sy-datum.
        w_zlest0203-usnam         = sy-uname.
        APPEND w_zlest0203 TO t_zlest0203.
      ENDIF.

      CLEAR: w_zlest0202, w_zlest0203.
    ENDLOOP.

  ENDIF.

  IF t_zlest0202[] IS NOT INITIAL.
    MODIFY zlest0202 FROM TABLE t_zlest0202.
    COMMIT WORK.
  ENDIF.

  IF t_zlest0203[] IS NOT INITIAL.
    MODIFY zlest0203 FROM TABLE t_zlest0203.
    COMMIT WORK.
  ENDIF.

  CLEAR: v_xml. REFRESH: t_zlest0202.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_TRANSFORMAR_TAB_XML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_transformar_tab_xml .

  DATA: l_conv      TYPE REF TO cl_abap_conv_in_ce.

  TRY.

* Remove namespace
      CALL TRANSFORMATION zles_remove_namespace_xslt
      SOURCE XML v_xml
      RESULT XML v_xml.

      CALL METHOD cl_abap_conv_in_ce=>create
        EXPORTING
          input = v_xml "(Xstring variable with XML)
        RECEIVING
          conv  = l_conv.

      CALL METHOD l_conv->read
        IMPORTING
          data = v_string. "(String variable with XML Encoding UTF-8)

    CATCH cx_transformation_error .
    CATCH cx_root .                                      "#EC CATCH_ALL

  ENDTRY.

  REPLACE ALL OCCURRENCES OF '<Movimento>' IN v_string WITH '<Movimento xmlns="SDPTup.xsd">'.

* Transforma o String concatenado em binário
  CLEAR v_xml.
  DATA(lo_conv) = cl_abap_conv_out_ce=>create( ).
  lo_conv->write( data = v_string ).
  v_xml = lo_conv->get_buffer( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_J1B3N
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_SAIDA  text
*----------------------------------------------------------------------*
FORM zf_call_transaction_j1b3n   USING p_saida TYPE y_saida.

  SET PARAMETER ID 'JEF' FIELD p_saida-docnum.
  CALL TRANSACTION 'J1B3N' AND SKIP FIRST SCREEN.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_PERIODO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_valida_periodo CHANGING p_disabled .

  DATA: l_days     LIKE  vtbbewe-atage,
        l_months   LIKE  vtbbewe-atage,
        l_years    LIKE  vtbbewe-atage,
        l_dtinicio TYPE sy-datum,
        l_dtfim    TYPE sy-datum.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      i_date_from = p_descfi
*     I_KEY_DAY_FROM       =
      i_date_to   = p_descff
*     I_KEY_DAY_TO         =
*     I_FLG_SEPARATE       = ' '
    IMPORTING
      e_days      = l_days
      e_months    = l_months
      e_years     = l_years.

  IF l_months > 1.
    MESSAGE s000(z_les) WITH 'Botão Gerar XML inativo.' 'Diferentes meses selecionados'
    DISPLAY LIKE 'W'.
    p_disabled = 'X'.
  ELSE.

    l_dtinicio = p_descfi(6) && 01.

    CALL FUNCTION 'FKK_LAST_DAY_OF_MONTH'
      EXPORTING
        day_in            = l_dtinicio
      IMPORTING
        last_day_of_month = l_dtfim
      EXCEPTIONS
        day_in_no_date    = 1
        OTHERS            = 2.

    IF l_dtfim <> p_descff.
      MESSAGE s000(z_les) WITH 'Botão Gerar XML inativo.' 'Período incompleto!'
                DISPLAY LIKE 'W'.
      p_disabled = 'X'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_AJUSTAR_HORA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_STARTTIME  text
*      -->P_TYPE  text
*      -->P_T  text
*      -->P_P_STARTDATE  text
*      -->P_TYPE  text
*      -->P_D  text
*      -->P_P_ADDTIME  text
*      -->P_TYPE  text
*      -->P_T  text
*      <--P_P_E_ENDTIME  text
*      <--P_TYPE  text
*      <--P_T  text
*      <--P_P_E_ENDDATE  text
*      <--P_TYPE  text
*      <--P_D  text
*      <--P_P_SINAL  text
*      <--P_TYPE  text
*      <--P_C  text
*----------------------------------------------------------------------*
FORM zf_ajustar_hora  USING     p_startdate TYPE  d
                                p_starttime TYPE  t
                                p_addtime   TYPE  t
                                p_sinal TYPE c
                                CHANGING
                                p_e_enddate TYPE  d
                                p_e_endtime TYPE  t.


  DATA:  l_differenz TYPE i.

*-------------------------------------------------------
* Adiciona minutos a uma hora informada
*-------------------------------------------------------
  IF p_sinal = '+'.
    p_e_endtime = p_starttime + p_addtime.
    l_differenz = p_e_endtime - p_starttime.

    IF l_differenz < 0.
      p_e_enddate = p_startdate + 1.
    ELSE.
      p_e_enddate = p_startdate.
    ENDIF.

  ENDIF.

*-------------------------------------------------------
* Subtrai minutos da hora informada
*-------------------------------------------------------
  IF p_sinal = '-'.
    p_e_endtime = p_starttime - p_addtime.
    l_differenz = p_e_endtime - p_starttime.

    IF l_differenz > 0.
      p_e_enddate = p_startdate - 1.
    ELSE.
      p_e_enddate = p_startdate.
    ENDIF.
  ENDIF.

ENDFORM.
