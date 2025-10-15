*  ----------------------------------------------------------------------*
*   ID........:                                                          *
*   Programa..: ZPPR023                                                  *
*   Tipo......: R - Report                                               *
*   Transação.: ZPM0097                                                  *
*   Descrição.: Aprovação de Alteração Lista Técnica                     *
*   Autor.....: Leandro Valentim Ferreira                                *
*   Data......: 27.03.2023                                               *
*  ----------------------------------------------------------------------*
*                       Controle de Alterações                           *
*  ----------------------------------------------------------------------*
*   Data       | Change     | Autor        | Alteração                   *
*  ----------------------------------------------------------------------*
*   27.03.23   |            |LVF           | Codificação Inicial         *
*  ----------------------------------------------------------------------*
REPORT zppr023.



TABLES: mast, zppt_controle, stpo, zppt_inter_equip, zppt_loc_instal, zppt_plano_manu.

*  &---------------------------------------------------------------------*
*  &     Declaração de Tipos
*  &---------------------------------------------------------------------*
TYPES: BEGIN OF y_saida,
         icon(5)         TYPE c,
         status(10)      TYPE  c, "ZDEPP_STAT_APROV,
         cod_alt         TYPE  zdepp_cod_list,
         equnr           TYPE  equnr,
         shtxt           TYPE  ktx01,
         herst           TYPE  herst,
         typbz           TYPE  typbz,
         rbnr            TYPE  rbnr,
         fleet_cat       TYPE  fleet_cat,
         chassis_num     TYPE  chassis_num,
         license_num     TYPE  license_num,
         status_email(5) TYPE  c,
         icon_email(5)   TYPE  c,
         aenam           TYPE  aenam,
         aedat           TYPE  aedat,
         hr_alteracao    TYPE  zdepp_hr_alt,
         aprovador       TYPE  zdepp_aprovador,
         dt_aprovacao    TYPE  zdepp_dt_aprov,
         hr_aprovacao    TYPE  zdepp_hr_aprov,
         color_line(4)   TYPE  c,
         color_cell      TYPE  lvc_t_scol,
       END OF y_saida.
*  &---------------------------------------------------------------------*
*  &     ALV
*  &---------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.

*  &---------------------------------------------------------------------*
*   Declaração de Instância de Métodos
*  &---------------------------------------------------------------------*
DATA: v_event_receiver  TYPE REF TO lcl_event_receiver.

*  &---------------------------------------------------------------------*
*  &  Declaração de Container
*  &---------------------------------------------------------------------*
DATA: v_container_h TYPE REF TO cl_gui_container,
      v_container_i TYPE REF TO cl_gui_container.

*  &---------------------------------------------------------------------*
*  &  Declaração de GRID
*  &---------------------------------------------------------------------*
DATA: v_grid     TYPE REF TO cl_gui_alv_grid.

*  &---------------------------------------------------------------------*
*  &  Docking
*  &---------------------------------------------------------------------*
DATA: v_docking  TYPE REF TO cl_gui_docking_container,
      v_splitter TYPE REF TO cl_gui_splitter_container.

DATA: t_zppt_inter_equip TYPE TABLE OF zppt_inter_equip,
      t_zppt_plano_manu  TYPE TABLE OF zppt_plano_manu,
      t_zppt_loc_instal  TYPE TABLE OF zppt_loc_instal,
      t_zppt_controle    TYPE TABLE OF zppt_controle,
      t_dd07t            TYPE TABLE OF dd07t,
      t_saida            TYPE TABLE OF zppe_inter_equip_alv, "Equipamento
      t_saida1           TYPE TABLE OF zppe_plano_manu_alv, "Plano Manutenção
      t_saida2           TYPE TABLE OF zppe_loc_instal_alv. "Instalação

DATA: t_coltab2 TYPE lvc_t_scol.

*  &---------------------------------------------------------------------*
*   Declaração de Ranges
*  &---------------------------------------------------------------------*
DATA: r_status TYPE RANGE OF zppt0025-status WITH HEADER LINE.

*  &---------------------------------------------------------------------*
*   Declaração de Estrutura
*  &---------------------------------------------------------------------*
DATA: w_disvariant TYPE disvariant,
      w_layout     TYPE lvc_s_layo,
      w_saida      LIKE LINE OF t_saida,
      w_saida1     LIKE LINE OF t_saida1,
      w_saida2     LIKE LINE OF t_saida2.

*  &---------------------------------------------------------------------*
*   Declaração de Variáveis
*  &---------------------------------------------------------------------*
DATA: v_info  TYPE c,
      v_ucomm TYPE sy-ucomm.
*  ----------------------------------------------------------------------*
*   Definição de Icones
*  ----------------------------------------------------------------------*
CONSTANTS: BEGIN OF gc_icones,
             icon_green_light(5)   TYPE c VALUE '@08@',
             icon_yellow_light(5)  TYPE c VALUE '@09@',
             icon_red_light(5)     TYPE c VALUE '@0A@',
             icon_system_okay(5)   TYPE c VALUE '@2K@',
             icon_system_cancel(5) TYPE c VALUE '@2O@',
           END OF gc_icones.

CONSTANTS: BEGIN OF gc,
             amarelo TYPE lvc_s_colo-col      VALUE '3',
             padrao  TYPE lvc_s_colo-col      VALUE '2',
             verde   TYPE lvc_s_colo-col      VALUE '5',
             int     TYPE lvc_s_colo-int      VALUE '1',
             inv     TYPE lvc_s_colo-inv      VALUE '0',
           END OF gc.


CONSTANTS: c_comtyp_int TYPE somlreci1-com_type VALUE 'INT'.

DATA: tl_objtxt   TYPE TABLE OF solisti1,
      tl_objpack  TYPE TABLE OF sopcklsti1,
      tl_receiver TYPE TABLE OF somlreci1.

DATA: wl_docdata  TYPE sodocchgi1,
      wl_objtxt   LIKE LINE OF tl_objtxt,
      wl_objpack  LIKE LINE OF tl_objpack,
      wl_receiver LIKE LINE OF tl_receiver.

DATA: lv_bukrs            TYPE bukrs,
      lv_rec_cnt          TYPE c LENGTH 10,
      lv_lines            TYPE i,
      lv_sentall          TYPE sonv-flag,
      lv_nobjid           TYPE sofolenti1-object_id,
      lv_aedat(10)        TYPE c,
      lv_hr_alteracao(10) TYPE c,
      lv_qtd_antiga(20)   TYPE c,
      lv_qtd_atual(20)    TYPE c.

DATA: send_request  TYPE REF TO cl_bcs,
      sent_to_all   TYPE os_boolean,
      document      TYPE REF TO cl_document_bcs,
      recipient     TYPE REF TO if_recipient_bcs,
      bcs_exception TYPE REF TO cx_bcs,

      wl_subject2   TYPE so_obj_des,
      wl_subject    TYPE string.

*  ----------------------------------------------------------------------*
*   Parâmetros de Seleção
*  ----------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK bl3 WITH FRAME TITLE TEXT-034.
  SELECTION-SCREEN BEGIN OF LINE.

***  SELECTION-SCREEN COMMENT 1(8) text-005.

    SELECTION-SCREEN POSITION 33.
    PARAMETERS: r_ie03   RADIOBUTTON GROUP g2 USER-COMMAND fc DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 35(12) TEXT-031.

    SELECTION-SCREEN POSITION 52.
    PARAMETERS: r_ip03 RADIOBUTTON GROUP g2.
    SELECTION-SCREEN COMMENT 54(19) TEXT-033.

***  SELECTION-SCREEN POSITION 76 .
***  PARAMETERS: r_il03 RADIOBUTTON GROUP g2.
***  SELECTION-SCREEN COMMENT 79(19) text-032.

  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK  bl3.

SELECTION-SCREEN: BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: s_equnr  FOR zppt_inter_equip-equnr MODIF ID md1,
                  s_tplnr  FOR zppt_loc_instal-tplnr  MODIF ID md2,
                  s_warpl  FOR zppt_plano_manu-warpl  MODIF ID md3,
                  s_werks  FOR mast-werks,
                  s_aedat  FOR sy-datum,
                  s_aprov  FOR zppt_controle-aprovador NO INTERVALS NO-EXTENSION.

  SELECTION-SCREEN BEGIN OF LINE.

    SELECTION-SCREEN COMMENT 1(8) TEXT-005.

    SELECTION-SCREEN POSITION 33.
    PARAMETERS: r_opcao1   RADIOBUTTON GROUP g1 DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 35(10) TEXT-003. " FOR FIELD  r_opcao1.

    SELECTION-SCREEN POSITION 52.
    PARAMETERS: r_opcao2 RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 54(10) TEXT-002. " FOR FIELD r_opcao2.

    SELECTION-SCREEN POSITION 73.
    PARAMETERS: r_opcao3 RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 75(10) TEXT-006. " FOR FIELD r_opcao2.

  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK  bl1.

SELECTION-SCREEN: BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-004.

SELECTION-SCREEN: END OF BLOCK  bl2.

AT SELECTION-SCREEN OUTPUT.
  PERFORM zf_screen.


START-OF-SELECTION.

  PERFORM zf_buscar_dados.

*  &---------------------------------------------------------------------*
*  &      Form  ZF_BUSCA_DADOS
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_buscar_dados .

  PERFORM zf_atualiza_tabelas. "FF 12.02.24 - USER STORY 78112

  PERFORM zf_clear.
  PERFORM zf_buscar_objetos_modificados.
  PERFORM zf_preparar_saida.

  IF t_saida[] IS NOT INITIAL OR
     t_saida1[] IS NOT INITIAL OR
     t_saida2[] IS NOT INITIAL.
    CALL SCREEN 9000.
  ELSE.
    MESSAGE i000(zppr) WITH 'Dados não encontrado.'.
  ENDIF.

ENDFORM.


*  ----------------------------------------------------------------------*
*         CLASS lcl_event_receiver DEFINITION
*  ----------------------------------------------------------------------*
*         Definição da classe lcl_event_receiver
*  ----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS:

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object
                  e_interactive.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*  ----------------------------------------------------------------------*
*         CLASS lcl_event_receiver IMPLEMENTATION
*  ----------------------------------------------------------------------*
*         Implementação da classe lcl_event_receiver
*  ----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_hotspot_click.
    PERFORM zf_handle_hotspot_click USING e_row_id-index e_column_id.
  ENDMETHOD.                    "handle_hotspot_click

  METHOD handle_user_command.

    PERFORM zf_buscar_aprovador.
    CLEAR v_ucomm.

    CASE e_ucomm.
      WHEN 'REJ'.
        v_ucomm = 'REJ'.
        PERFORM zf_rejeitar_modific.
      WHEN 'REF'.
        PERFORM zf_atualiza_tabelas. "FF 12.02.24 - USER STORY 78112
        PERFORM zf_buscar_objetos_modificados.
        PERFORM zf_preparar_saida.
      WHEN 'APROVAR'.
        PERFORM zf_efetuar_aprovacao.
        v_ucomm = e_ucomm.
      WHEN 'CANCAPROV'.
        PERFORM zf_cancelar_aprovacao.
      WHEN 'EMAIL'.
        PERFORM zf_reenvio_email.
      WHEN 'INFO'.
        PERFORM zf_expand_legenda.
      WHEN 'CADEMAIL'.
        CASE abap_true.
          WHEN r_ie03."Equipamento
            PERFORM zf_cad_email_equip.
***            WHEN r_il03."Instalação
***              PERFORM zf_cad_email_instal.
          WHEN r_ip03."Planos Manutenção
            PERFORM zf_cad_email_manut.
        ENDCASE.
      WHEN OTHERS.
    ENDCASE.

    CALL METHOD v_grid->refresh_table_display.

  ENDMETHOD.                    "handle_user_command

  METHOD handle_toolbar.

    PERFORM zf_adiciona_botoes_header USING e_object.
    PERFORM zf_elimina_botoes_header  USING e_object.

  ENDMETHOD.                    "handle_toolbar

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


*  &---------------------------------------------------------------------*
*  &      Form  ZF_ADICIONA_BOTOES_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_OBJECT  text
*  ----------------------------------------------------------------------*
FORM zf_adiciona_botoes_header USING e_object TYPE REF TO cl_alv_event_toolbar_set.

*   Add Button
  DATA: w_toolbar  TYPE stb_button.

*  ----------------------------------------------------------------------*
*   legendas
  CLEAR w_toolbar.
  MOVE 3 TO w_toolbar-butn_type.
  APPEND w_toolbar TO e_object->mt_toolbar.

  CLEAR w_toolbar.
  MOVE 'INFO'                 TO w_toolbar-function.
  MOVE icon_information       TO w_toolbar-icon.
  MOVE '0 '                   TO w_toolbar-butn_type.
  MOVE 'Legendas'(028)        TO w_toolbar-quickinfo.
  APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Aprovar
  CLEAR w_toolbar.

  MOVE 'APROVAR'                 TO w_toolbar-function.
  MOVE icon_set_state            TO w_toolbar-icon.
  MOVE '0 '                      TO w_toolbar-butn_type.
  MOVE 'Aprovar'(008)            TO w_toolbar-quickinfo.
  MOVE 'Aprovar'(008)            TO w_toolbar-text.
  IF r_opcao1 IS INITIAL.
    MOVE r_opcao2 TO w_toolbar-disabled.
  ENDIF.
  APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Cancelar Aprovação
  CLEAR w_toolbar.

  MOVE 'CANCAPROV'                 TO w_toolbar-function.
  MOVE icon_cancel                 TO w_toolbar-icon.
  MOVE '0 '                        TO w_toolbar-butn_type.
  MOVE 'Cancelar Aprovação/Rejeitar'(009)   TO w_toolbar-quickinfo.
  MOVE 'Cancelar Aprovação/Rejeitar'(009)   TO w_toolbar-text.
  MOVE r_opcao1 TO w_toolbar-disabled.
  APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Reenviar e-mail
  CLEAR w_toolbar.

  MOVE 'EMAIL'                    TO w_toolbar-function.
  MOVE icon_mail                 TO w_toolbar-icon.
  MOVE '0 '                      TO w_toolbar-butn_type.
  MOVE 'Reenvio E-mail'(010)     TO w_toolbar-quickinfo.
  MOVE 'Reenvio E-mail'(010)     TO w_toolbar-text.
  APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Cancelar Aprovação
  CLEAR w_toolbar.

  MOVE 'CADEMAIL'                 TO w_toolbar-function.
  MOVE icon_display              TO w_toolbar-icon.
  MOVE '0 '                        TO w_toolbar-butn_type.
  MOVE 'Consultar aprovador'(030)   TO w_toolbar-quickinfo.
  MOVE 'Consultar Aprovador'(030)   TO w_toolbar-text.
  IF r_opcao2 IS INITIAL.
    MOVE r_opcao2 TO w_toolbar-disabled.
  ENDIF.
  APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Atualizar
  CLEAR w_toolbar.

  MOVE 'REJ'              TO w_toolbar-function.
  MOVE icon_cancel        TO w_toolbar-icon.
  MOVE '0 '               TO w_toolbar-butn_type.
  MOVE 'Rejeitar'(035)    TO w_toolbar-quickinfo.
  MOVE 'Rejeitar'(035)    TO w_toolbar-text.
  IF r_opcao1 IS INITIAL.
    MOVE r_opcao2 TO w_toolbar-disabled.
  ENDIF.
  APPEND w_toolbar TO e_object->mt_toolbar.

*  ----------------------------------------------------------------------*
*   Atualizar
  CLEAR w_toolbar.

  MOVE 'REF'              TO w_toolbar-function.
  MOVE icon_refresh       TO w_toolbar-icon.
  MOVE '0 '               TO w_toolbar-butn_type.
  MOVE 'Atualizar'(034)   TO w_toolbar-quickinfo.
  MOVE 'Atualizar'(034)   TO w_toolbar-text.
  IF r_opcao2 IS INITIAL.
    MOVE r_opcao2 TO w_toolbar-disabled.
  ENDIF.
  APPEND w_toolbar TO e_object->mt_toolbar.


ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_ELIMINA_BOTOES_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_OBJECT  text
*  ----------------------------------------------------------------------*
FORM zf_elimina_botoes_header  USING  e_object TYPE REF TO cl_alv_event_toolbar_set.

*      elimina itens desnecessarios da barra do container
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
                                 OR function = '&MB_VARIANT'
*                                   OR FUNCTION =  '&MB_EXPORT'
                                 OR function =  '&MB_SUM'
                                 OR function =  '&MB_SUBTOT'
                                 OR function =  '&PRINT_BACK'.
ENDFORM.

*  &---------------------------------------------------------------------*
*  &      Form  ZF_BUSCAR_APROVADOR
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_buscar_aprovador .

  REFRESH: t_zppt_controle.

  CASE abap_true.
    WHEN r_ie03."Equipamento
      SELECT * FROM zppt_controle
        INTO TABLE t_zppt_controle
        FOR ALL ENTRIES IN t_zppt_inter_equip
        WHERE werks = t_zppt_inter_equip-werks.
***      WHEN r_il03."Instalação
***        SELECT * FROM zppt_controle
***          INTO TABLE t_zppt_controle
***          FOR ALL ENTRIES IN t_zppt_loc_instal
***          WHERE werks = t_zppt_loc_instal-werks.
    WHEN r_ip03."Planos Manutenção
      SELECT * FROM zppt_controle
        INTO TABLE t_zppt_controle
        FOR ALL ENTRIES IN t_zppt_plano_manu
        WHERE werks = t_zppt_plano_manu-werks.
  ENDCASE.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_BUSCAR_OBJETOS_MODIFICADOS
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_buscar_objetos_modificados .

  REFRESH: r_status.

  r_status-sign   = 'I'.
  r_status-option = 'EQ'.

  CASE abap_true.
    WHEN r_opcao1.
      r_status-low    = 'P'.
      APPEND r_status.
    WHEN r_opcao2.
      r_status-low    = 'A'.
      APPEND r_status.
    WHEN r_opcao3.

      r_status-low    = 'A'.
      APPEND r_status.

      r_status-low    = 'P'.
      APPEND r_status.

      r_status-low    = 'R'.
      APPEND r_status.

    WHEN OTHERS.
  ENDCASE.

  REFRESH: t_zppt_inter_equip,t_zppt_loc_instal,t_zppt_plano_manu.

  CASE abap_true.
    WHEN r_ie03.
      SELECT *
             INTO TABLE t_zppt_inter_equip
             FROM zppt_inter_equip
             WHERE equnr IN s_equnr
               AND werks IN s_werks
               AND aedat IN s_aedat
               AND status IN r_status.

      DATA(lv_subrc) = sy-subrc.
***      WHEN r_il03.
***        SELECT *
***              INTO TABLE t_zppt_loc_instal
***              FROM  zppt_loc_instal
***              WHERE tplnr IN s_tplnr
***                AND werks IN s_werks
***                AND aedat IN s_aedat
***                AND aprovador IN s_aprov
***                AND status IN r_status.
***
***        lv_subrc = sy-subrc.
    WHEN r_ip03.
      SELECT *
            INTO TABLE t_zppt_plano_manu
            FROM  zppt_plano_manu
            WHERE warpl IN s_warpl
              AND werks IN s_werks
              AND aedat IN s_aedat
              AND status IN r_status.
      lv_subrc = sy-subrc.
  ENDCASE.

  IF lv_subrc IS NOT INITIAL AND v_ucomm <> 'APROVAR'.
    MESSAGE s000(zppr) WITH 'Nenhum registro encontrado.' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_PREPARAR_SAIDA
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
FORM zf_preparar_saida .

  REFRESH: t_saida,t_saida1,t_saida2, t_dd07t.

*   DD-R/3: valores dos domínios
  SELECT * FROM dd07t
    INTO TABLE t_dd07t
    WHERE domname = 'ZDOPP_STAT_APROV'.

  PERFORM zf_define_cor_colunas.

  CASE abap_true.
    WHEN r_ie03."Equipamento
      PERFORM zf_saida_equi.
***      WHEN r_il03."Instalação
***        PERFORM zf_saida_inst.
    WHEN r_ip03."Planos Manutenção
      PERFORM zf_saida_plan.
  ENDCASE.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Module  ZM_STATUS_9000  OUTPUT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
MODULE zm_status_9000 OUTPUT.
  SET PF-STATUS 'ZGPP_STATUS_9000'.
  SET TITLEBAR  'ZUPP_TITULO'.

  PERFORM zf_split_screen.

  CASE abap_true.
    WHEN r_ie03."Equipamento
      PERFORM zf_preparar_alv USING t_saida.
***      WHEN r_il03."Instalação
***        PERFORM zf_preparar_alv USING t_saida2.
    WHEN r_ip03."Planos Manutenção
      PERFORM zf_preparar_alv USING t_saida1.
  ENDCASE.

ENDMODULE.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_SPLIT_SCREEN
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_split_screen .

  CLEAR: v_docking, v_splitter, v_container_h, v_container_i.
  CREATE OBJECT v_docking
    EXPORTING
      repid = sy-repid
      dynnr = sy-dynnr
      ratio = '95'.

*   Create a splitter with 2 rows and 1 column
  CREATE OBJECT v_splitter
    EXPORTING
      parent  = v_docking
      rows    = 2
      columns = 1.

*  * Upper Container
  CALL METHOD v_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = v_container_h.

*  * Lower Container
  CALL METHOD v_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = v_container_i.

*  * Upper Container height

  CALL METHOD v_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 20.

  PERFORM zf_preparar_header.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_PREPARAR_HEADER
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_preparar_header .

  DATA: l_document  TYPE REF TO cl_dd_document,
        l_doctable  TYPE REF TO cl_dd_table_element,
        l_column1   TYPE REF TO cl_dd_area,
        l_column2   TYPE REF TO cl_dd_area,
        l_text(255) TYPE c.  "Text

  CREATE OBJECT l_document.
*  ************ - Titulo Legenda
  CLEAR l_text.
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_text
    EXPORTING
      text         = 'Legenda'(023)
      sap_emphasis = 'STRONG'.

  CALL METHOD l_document->new_line.

*  *************L1 - Legenda
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_GREEN_LIGHT'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Aprovada'(024).

  CALL METHOD l_document->new_line.

*  *************L3 - Aguardando aprovação
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_YELLOW_LIGHT'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Aguardando aprovação'(025).

  CALL METHOD l_document->new_line.

*  *************L4 - Email enviado com sucesso
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_SYSTEM_OKAY'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Email enviado com sucesso'(026).

  CALL METHOD l_document->new_line.

*  *************L5 - Erro no envio do mail
  CALL METHOD l_document->add_gap
    EXPORTING
      width = 30.

  CALL METHOD l_document->add_icon
    EXPORTING
      sap_icon = 'ICON_SYSTEM_CANCEL'.

  CALL METHOD l_document->add_text
    EXPORTING
      text = 'Erro envio de Email'(027).

  CALL METHOD l_document->new_line.

*  ********************************************
  CALL METHOD l_document->display_document
    EXPORTING
      parent = v_container_h.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_PREPARAR_ALV
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_preparar_alv USING p_saida TYPE ANY TABLE.

  DATA: t_fcat TYPE lvc_t_fcat,
        t_sort TYPE lvc_t_sort.

  DATA: w_variant TYPE disvariant,
        w_layout  TYPE lvc_s_layo.

  w_layout-cwidth_opt = 'X'.
  w_layout-zebra      = 'X'.
  w_layout-info_fname = 'COLOR_LINE'.
  w_layout-ctab_fname = 'COLOR_CELL'.

*    W_LAYOUT-STYLEFNAME = 'CELLTAB'.
*    W_LAYOUT-SEL_MODE   = 'A'.

  w_variant-report    = sy-repid.
*    W_VARIANT-VARIANT   = P_LAYOUT.

  PERFORM zf_montar_fieldcat CHANGING p_saida t_fcat.
  PERFORM zf_ajuste_fieldcat CHANGING t_fcat.
*    PERFORM ZF_AJUSTE_SAIDA.

  IF v_grid IS INITIAL.

    CREATE OBJECT v_grid
      EXPORTING
        i_parent          = v_container_i
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

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
        it_outtab                     = p_saida
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

*     RAISE EVENT TOOLBAR TO SHOW THE MODIFIED TOOLBAR
    CALL METHOD v_grid->set_toolbar_interactive.

  ELSE.
    CALL METHOD v_grid->refresh_table_display.
  ENDIF.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_MONTAR_FIELDCAT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        <--P_T_SAIDA  text
*        <--P_T_FCAT  text
*  ----------------------------------------------------------------------*
FORM zf_montar_fieldcat CHANGING pt_tabela   TYPE ANY TABLE
                                  pt_fieldcat TYPE lvc_t_fcat.

  DATA:
    l_columns      TYPE REF TO cl_salv_columns_table,
    l_aggregations TYPE REF TO cl_salv_aggregations,
    l_salv_table   TYPE REF TO cl_salv_table,
    l_data         TYPE REF TO data.

*-- cl_salv_table data
  DATA: gr_table     TYPE REF TO cl_salv_table.
  DATA: gr_columns   TYPE REF TO cl_salv_columns_table.
  DATA: gr_column    TYPE REF TO cl_salv_column_table.
  DATA: gr_functions TYPE REF TO cl_salv_functions.
  DATA: gr_events    TYPE REF TO cl_salv_events_table.


  FIELD-SYMBOLS:
    <f_table>      TYPE STANDARD TABLE.

*   Cria uma estrutura com o mesmo layout da tabela de saída
  CREATE DATA l_data LIKE pt_tabela.
  ASSIGN l_data->* TO <f_table>.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

*   Monta a estrutura dinâmica no objeto l_salv_table
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

*   Recupera as colunas e dados internos
  l_columns      = l_salv_table->get_columns( ).
  l_aggregations = l_salv_table->get_aggregations( ).

*   Monta o fieldcat
  pt_fieldcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = l_columns
                                                                   r_aggregations = l_aggregations ).

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Module  ZM_EXIT  INPUT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
MODULE zm_exit INPUT.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_EFETUAR_APROVACAO
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
FORM zf_efetuar_aprovacao .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,

        w_0025       TYPE zppt0025.

  REFRESH: t_zppt_inter_equip,t_zppt_plano_manu,t_zppt_loc_instal.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE abap_true.
    WHEN r_ie03."Equipamento
      PERFORM zf_aprova_equip TABLES t_row_no.
***      WHEN r_il03."Instalação
***        PERFORM zf_aprova_instal TABLES t_row_no.
    WHEN r_ip03."Planos Manutenção
      PERFORM zf_aprova_manut TABLES t_row_no.
  ENDCASE.

  PERFORM zf_buscar_objetos_modificados.
  PERFORM zf_preparar_saida.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_AJUSTE_FIELDCAT
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        <--P_T_FCAT  text
*  ----------------------------------------------------------------------*
FORM zf_ajuste_fieldcat  CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).

    <fs_fcat>-coltext = <fs_fcat>-reptext.

    CASE <fs_fcat>-fieldname.
      WHEN 'MATNR'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Lista técnica'.
        <fs_fcat>-hotspot = 'X'.
      WHEN 'IDNRK'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Componente'.
      WHEN 'TIPO'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Tipo'.
      WHEN 'STATUS'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Status'.
      WHEN 'STATUS_EMAIL'.
        <fs_fcat>-no_out = 'X'.
      WHEN 'ICON_EMAIL'.
        <fs_fcat>-coltext = <fs_fcat>-reptext = 'Email'.
      WHEN 'STATUS'.
        <fs_fcat>-drdn_hndl  = '1'.
      WHEN 'WARPL' OR 'EQUNR'. "FF 12.02.24 - USER STORY 78112
        <fs_fcat>-hotspot = 'X'.
      WHEN OTHERS.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_CANCELAR_APROVACAO
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_cancelar_aprovacao .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,

        w_0025       TYPE zppt0025.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE abap_true.
    WHEN r_ie03."Equipamento
      PERFORM zf_cancel_ap_equip TABLES t_row_no.
***      WHEN r_il03."Instalação
***        PERFORM zf_cancel_ap_instal TABLES t_row_no.
    WHEN r_ip03."Planos Manutenção
      PERFORM zf_cancel_ap_manut TABLES t_row_no.
  ENDCASE.

  PERFORM zf_buscar_objetos_modificados.
  PERFORM zf_preparar_saida.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_REENVIO_EMAIL
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_reenvio_email .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,

        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,

        l_enviado    TYPE sofolenti1-object_id.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE abap_true.
    WHEN r_ie03."Equipamento
      PERFORM zf_reenvio_email_equip TABLES t_row_no.
***      WHEN r_il03."Instalação
***        PERFORM zf_reenvio_email_instal TABLES t_row_no.
    WHEN r_ip03."Planos Manutenção
      PERFORM zf_reenvio_email_manut TABLES t_row_no.
  ENDCASE.

  PERFORM zf_buscar_objetos_modificados.
  PERFORM zf_preparar_saida.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_EXPAND_LEGENDA
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_expand_legenda .

  IF v_info IS INITIAL.
    CALL METHOD v_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 27.

    v_info = 'X'.
  ELSE.
    CALL METHOD v_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 0.

    CLEAR v_info.
  ENDIF.

  CALL METHOD v_grid->refresh_table_display.


ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_CAD_EMAIL
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_cad_email.


ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_CS03
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_cs03 USING p_valor p_param p_tcode.

  SET PARAMETER ID: p_param FIELD p_valor.
  CALL TRANSACTION p_tcode AND SKIP FIRST SCREEN.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_HANDLE_HOTSPOT_CLICK
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*        -->P_E_ROW_ID_INDEX  text
*        -->P_E_COLUMN_ID  text
*  ----------------------------------------------------------------------*
FORM zf_handle_hotspot_click    USING    p_index  TYPE any
                                       p_column TYPE any.


  CASE abap_true.
    WHEN r_ie03."Equipamento
      READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX p_index.
***      WHEN r_il03."Instalação
***        READ TABLE t_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida2>) INDEX p_index.
    WHEN r_ip03."Planos Manutenção
      READ TABLE t_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida1>) INDEX p_index.
  ENDCASE.

  CHECK sy-subrc IS INITIAL.

  CASE p_column.
    WHEN 'EQUNR'.
      PERFORM zf_cs03 USING <fs_saida>-equnr 'EQN' 'IE03'.
***      WHEN 'TPLNR'.
***        PERFORM zf_cs03 USING <fs_saida2>-tplnr 'IFL' 'IL03'.
    WHEN 'WARPL'.
      PERFORM zf_cs03 USING <fs_saida1>-warpl 'MPL' 'IP03'.
  ENDCASE.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_AJUSTE_SAIDA
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_ajuste_saida .

  LOOP AT t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
    <fs_saida>-color_cell = t_coltab2.
  ENDLOOP.

ENDFORM.
*  &---------------------------------------------------------------------*
*  &      Form  ZF_DEFINE_COR_COLUNAS
*  &---------------------------------------------------------------------*
*         text
*  ----------------------------------------------------------------------*
*    -->  p1        text
*    <--  p2        text
*  ----------------------------------------------------------------------*
FORM zf_define_cor_colunas .

  DATA: w_col   TYPE lvc_s_scol,
        w_color TYPE lvc_s_colo.

  REFRESH: t_coltab2.
*  ------------------------------------------------
*   Cor dos dados de alteração
*  ------------------------------------------------
***    w_color-col   = gc-verde.
***    w_color-int   = gc-int.
***    w_color-inv   = gc-inv.
***    w_col-fname   = 'AENAM'.  "Nome da coluna
***    w_col-color   = w_color.
***    APPEND w_col TO t_coltab2.

  w_color-col   = gc-verde.
  w_color-int   = gc-int.
  w_color-inv   = gc-inv.
  w_col-fname   = 'AEDAT'.  "Nome da coluna
  w_col-color   = w_color.
  APPEND w_col TO t_coltab2.

  w_color-col   = gc-verde.
  w_color-int   = gc-int.
  w_color-inv   = gc-inv.
  w_col-fname   = 'HR_ALTERACAO'.  "Nome da coluna
  w_col-color   = w_color.
  APPEND w_col TO t_coltab2.

*  ------------------------------------------------
*  cor dos dados de aprovação
*  ------------------------------------------------

  w_color-col   = gc-amarelo.
  w_color-int   = gc-int.
  w_color-inv   = gc-inv.
  w_col-fname   = 'APROVADOR'.  "Nome da coluna
  w_col-color   = w_color.
  APPEND w_col TO t_coltab2.

  w_color-col   = gc-amarelo.
  w_color-int   = gc-int.
  w_color-inv   = gc-inv.
  w_col-fname   = 'DT_APROVACAO'.  "Nome da coluna
  w_col-color   = w_color.
  APPEND w_col TO t_coltab2.

  w_color-col   = gc-amarelo.
  w_color-int   = gc-int.
  w_color-inv   = gc-inv.
  w_col-fname   = 'HR_APROVACAO'.  "Nome da coluna
  w_col-color   = w_color.
  APPEND w_col TO t_coltab2.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAIDA_EQUI
*&---------------------------------------------------------------------*
*       text
*&---------------------------------------------------------------------*
FORM zf_saida_equi .

  IF s_aprov[] IS NOT INITIAL.
    SELECT *
           INTO TABLE @DATA(tl_zppt_controle)
           FROM zppt_controle
           WHERE aprovador IN @s_aprov.

    SORT tl_zppt_controle BY werks eqtyp.

    IF sy-subrc EQ 0.
      SELECT *
             INTO TABLE @DATA(tl_equi)
             FROM equi
             FOR ALL ENTRIES IN @t_zppt_inter_equip
             WHERE equnr EQ @t_zppt_inter_equip-equnr.

      IF sy-subrc EQ 0.

        SORT tl_equi BY equnr.

        SELECT *
               INTO TABLE @DATA(tl_itob)
               FROM itob
               FOR ALL ENTRIES IN @tl_equi
               WHERE equnr EQ @tl_equi-equnr.

        SORT tl_itob BY equnr aedat DESCENDING.
        DELETE ADJACENT DUPLICATES FROM tl_itob COMPARING equnr aedat.
        SORT tl_itob BY equnr.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT t_zppt_inter_equip INTO DATA(wl_zppt_inter_equip).

    IF s_aprov[] IS NOT INITIAL.
      IF tl_zppt_controle[] IS NOT INITIAL.
        READ TABLE tl_equi INTO DATA(wl_equi) WITH KEY equnr = wl_zppt_inter_equip-equnr BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE tl_itob INTO DATA(wl_itob) WITH KEY equnr = wl_equi-equnr BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE tl_zppt_controle TRANSPORTING NO FIELDS WITH KEY werks = wl_itob-iwerk
                                                                        eqtyp = wl_equi-eqtyp BINARY SEARCH.
            IF sy-subrc NE 0.
              CONTINUE."A linha do arquivo não pertence ao aprovador informado na tela
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        CONTINUE."A linha do arquivo não pertence ao aprovador informado na tela
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING wl_zppt_inter_equip TO w_saida.

*     Texto do Status
    READ TABLE t_dd07t INTO DATA(w_dd07t) WITH KEY domvalue_l = w_saida-status.
    IF sy-subrc IS INITIAL.
      w_saida-status = w_dd07t-ddtext.
    ENDIF.

    CASE wl_zppt_inter_equip-status.
      WHEN 'A'.
        w_saida-icon   = gc_icones-icon_green_light.
      WHEN 'P'.
        w_saida-icon   = gc_icones-icon_yellow_light.
      WHEN 'R'.
        w_saida-icon   = gc_icones-icon_red_light.
      WHEN OTHERS.
    ENDCASE.

    IF wl_zppt_inter_equip-status_email = 'S'.
      w_saida-icon_email   = gc_icones-icon_system_okay.
    ELSE.
      w_saida-icon_email   = gc_icones-icon_system_cancel.
    ENDIF.

    w_saida-color_cell = t_coltab2.
    APPEND w_saida TO t_saida.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAIDA_INST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_saida_inst .
  LOOP AT t_zppt_loc_instal INTO DATA(wl_zppt_loc_instal).
    MOVE-CORRESPONDING wl_zppt_loc_instal TO w_saida2.

*     Texto do Status
    READ TABLE t_dd07t INTO DATA(w_dd07t) WITH KEY domvalue_l = w_saida2-status.
    IF sy-subrc IS INITIAL.
      w_saida2-status = w_dd07t-ddtext.
    ENDIF.

    CASE wl_zppt_loc_instal-status.
      WHEN 'A'.
        w_saida2-icon   = gc_icones-icon_green_light.
      WHEN 'P'.
        w_saida2-icon   = gc_icones-icon_yellow_light.
      WHEN 'R'.
        w_saida2-icon   = gc_icones-icon_red_light.
      WHEN OTHERS.
    ENDCASE.

    IF wl_zppt_loc_instal-status_email = 'S'.
      w_saida2-icon_email   = gc_icones-icon_system_okay.
    ELSE.
      w_saida2-icon_email   = gc_icones-icon_system_cancel.
    ENDIF.

    w_saida2-color_cell = t_coltab2.
    APPEND w_saida2 TO t_saida2.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SAIDA_PLAN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_saida_plan .

  IF s_aprov[] IS NOT INITIAL.
    SELECT *
           INTO TABLE @DATA(tl_zppt_controle)
           FROM zppt_controle
           WHERE aprovador IN @s_aprov.

    SORT tl_zppt_controle BY werks mptyp.

    IF sy-subrc EQ 0.
      SELECT *
             INTO TABLE @DATA(tl_mpla)
             FROM mpla
             FOR ALL ENTRIES IN @t_zppt_plano_manu
             WHERE warpl EQ @t_zppt_plano_manu-warpl.

      SORT tl_mpla BY warpl.
      IF sy-subrc EQ 0.
        SELECT *
               INTO TABLE @DATA(tl_mpos)
               FROM mpos
               FOR ALL ENTRIES IN @tl_mpla
               WHERE warpl EQ @tl_mpla-warpl.
        IF sy-subrc EQ 0.
          SORT tl_mpos BY warpl aedat DESCENDING.
          DELETE ADJACENT DUPLICATES FROM tl_mpos COMPARING warpl aedat.
          SORT tl_mpos BY warpl.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT t_zppt_plano_manu INTO DATA(wl_zppt_plano_manu).

    IF s_aprov[] IS NOT INITIAL.
      IF tl_zppt_controle[] IS NOT INITIAL.
        READ TABLE tl_mpla INTO DATA(wl_mpla) WITH KEY warpl = wl_zppt_plano_manu-warpl BINARY SEARCH.
        IF sy-subrc EQ 0.
          READ TABLE tl_mpos INTO DATA(wl_mpos) WITH KEY warpl = wl_mpla-warpl BINARY SEARCH.
          IF sy-subrc EQ 0.
            READ TABLE tl_zppt_controle TRANSPORTING NO FIELDS WITH KEY werks = wl_mpos-iwerk
                                                                        mptyp = wl_mpla-mptyp BINARY SEARCH.
            IF sy-subrc NE 0.
              CONTINUE."A linha do arquivo não pertence ao aprovador informado na tela
            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        CONTINUE."A linha do arquivo não pertence ao aprovador informado na tela
      ENDIF.
    ENDIF.

    MOVE-CORRESPONDING wl_zppt_plano_manu TO w_saida1.

*     Texto do Status
    READ TABLE t_dd07t INTO DATA(w_dd07t) WITH KEY domvalue_l = w_saida1-status.
    IF sy-subrc IS INITIAL.
      w_saida1-status = w_dd07t-ddtext.
    ENDIF.

    CASE wl_zppt_plano_manu-status.
      WHEN 'A'.
        w_saida1-icon   = gc_icones-icon_green_light.
      WHEN 'P'.
        w_saida1-icon   = gc_icones-icon_yellow_light.
      WHEN 'R'.
        w_saida1-icon   = gc_icones-icon_red_light.
      WHEN OTHERS.
    ENDCASE.

    IF wl_zppt_plano_manu-status_email = 'S'.
      w_saida1-icon_email   = gc_icones-icon_system_okay.
    ELSE.
      w_saida1-icon_email   = gc_icones-icon_system_cancel.
    ENDIF.

    w_saida1-color_cell = t_coltab2.
    APPEND w_saida1 TO t_saida1.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_APROVA_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_W_ROW_NO_ROW_ID  text
*----------------------------------------------------------------------*
FORM zf_aprova_equip  TABLES p_row_no TYPE lvc_t_roid.

  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL AND <fs_saida>-status(1) EQ 'P'.

    IF <fs_saida>-cod_alt IS NOT INITIAL.

      REFRESH: t_zppt_inter_equip.
      SELECT * FROM zppt_inter_equip
        INTO TABLE @DATA(t_zppt_inter_equip)
        WHERE cod_alt = @<fs_saida>-cod_alt.

      IF sy-subrc EQ 0.
        SELECT *
               FROM zppt_controle
               INTO TABLE t_zppt_controle
               WHERE werks EQ <fs_saida>-werks.
      ENDIF.
    ENDIF.

    LOOP AT t_zppt_inter_equip ASSIGNING FIELD-SYMBOL(<fs_zppt_inter_equip>).

      READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_inter_equip>-werks
                                                                     aprovador = sy-uname.

      IF sy-subrc IS INITIAL.
        <fs_zppt_inter_equip>-status       = 'A'.
        <fs_zppt_inter_equip>-aprovador    = sy-uname.
        <fs_zppt_inter_equip>-dt_aprovacao = sy-datum.
        <fs_zppt_inter_equip>-hr_aprovacao = sy-uzeit.
      ELSE.
        MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF t_zppt_inter_equip[] IS NOT INITIAL.
    MODIFY zppt_inter_equip FROM TABLE t_zppt_inter_equip.
    COMMIT WORK.

    MESSAGE s000(zppr) WITH 'Registro aprovado'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_APROVA_INSTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_aprova_instal  TABLES p_row_no TYPE lvc_t_roid.

  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL AND <fs_saida>-status <> 'A'.

    IF <fs_saida>-cod_alt IS NOT INITIAL.

      REFRESH: t_zppt_loc_instal.
      SELECT * FROM zppt_loc_instal
        INTO TABLE @DATA(t_zppt_loc_instal)
        WHERE cod_alt = @<fs_saida>-cod_alt.

      IF sy-subrc EQ 0.
        SELECT *
               FROM zppt_controle
               INTO TABLE t_zppt_controle
               WHERE werks EQ <fs_saida>-werks.
      ENDIF.
    ENDIF.

    LOOP AT t_zppt_loc_instal ASSIGNING FIELD-SYMBOL(<fs_zppt_loc_instal>).

      READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_loc_instal>-werks
                                                                     aprovador = sy-uname.

      IF sy-subrc IS INITIAL.

        <fs_zppt_loc_instal>-status       = 'A'.
        <fs_zppt_loc_instal>-aprovador    = sy-uname.
        <fs_zppt_loc_instal>-dt_aprovacao = sy-datum.
        <fs_zppt_loc_instal>-hr_aprovacao = sy-uzeit.

      ELSE.
        MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF t_zppt_loc_instal[] IS NOT INITIAL.
    MODIFY zppt_loc_instal FROM TABLE t_zppt_loc_instal.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_APROVA_MANUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_aprova_manut  TABLES p_row_no TYPE lvc_t_roid.
  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL AND <fs_saida>-status(1) EQ 'P'.

    IF <fs_saida>-cod_alt IS NOT INITIAL.

      REFRESH: t_zppt_plano_manu.
      SELECT * FROM zppt_plano_manu
        INTO TABLE @DATA(t_zppt_plano_manu)
        WHERE cod_alt = @<fs_saida>-cod_alt.

      IF sy-subrc EQ 0.
        SELECT *
               FROM zppt_controle
               INTO TABLE t_zppt_controle
               WHERE werks EQ <fs_saida>-werks.
      ENDIF.
    ENDIF.

    LOOP AT t_zppt_plano_manu ASSIGNING FIELD-SYMBOL(<fs_zppt_plano_manu>).

      READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_plano_manu>-werks
                                                                     aprovador = sy-uname.

      IF sy-subrc IS INITIAL.

        <fs_zppt_plano_manu>-status       = 'A'.
        <fs_zppt_plano_manu>-aprovador    = sy-uname.
        <fs_zppt_plano_manu>-dt_aprovacao = sy-datum.
        <fs_zppt_plano_manu>-hr_aprovacao = sy-uzeit.

      ELSE.
        MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF t_zppt_plano_manu[] IS NOT INITIAL.
    MODIFY zppt_plano_manu FROM TABLE t_zppt_plano_manu.
    COMMIT WORK.
    MESSAGE s000(zppr) WITH 'Registro aprovado'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CANCEL_AP_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_cancel_ap_equip  TABLES p_row_no TYPE lvc_t_roid.

  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL.

    IF <fs_saida>-status(1) EQ 'A' OR <fs_saida>-status(1) EQ 'R'.
      IF <fs_saida>-werks IS NOT INITIAL AND <fs_saida>-equnr IS NOT INITIAL.

        REFRESH: t_zppt_inter_equip.
        SELECT * FROM zppt_inter_equip
          INTO TABLE @DATA(t_zppt_inter_equip)
          WHERE cod_alt = @<fs_saida>-cod_alt
            AND werks   = @<fs_saida>-werks
            AND equnr   = @<fs_saida>-equnr.

        IF sy-subrc EQ 0.
          SELECT *
                 FROM zppt_controle
                 INTO TABLE t_zppt_controle
                 WHERE werks EQ <fs_saida>-werks.
        ENDIF.
      ENDIF.

      LOOP AT t_zppt_inter_equip ASSIGNING FIELD-SYMBOL(<fs_zppt_inter_equip>).

        READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_inter_equip>-werks
                                                                       aprovador = sy-uname.

        IF sy-subrc IS INITIAL.
          <fs_zppt_inter_equip>-status       = 'P'.
          CLEAR: <fs_zppt_inter_equip>-aprovador, <fs_zppt_inter_equip>-dt_aprovacao, <fs_zppt_inter_equip>-hr_aprovacao.
        ELSE.
          MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDLOOP.
    ELSE.
      MESSAGE i000(zppr) WITH 'Efetue a aprovação para cancelar'.
    ENDIF.
  ENDLOOP.

  IF t_zppt_inter_equip[] IS NOT INITIAL.
    MODIFY zppt_inter_equip FROM TABLE t_zppt_inter_equip.
    COMMIT WORK.
    MESSAGE s000(zppr) WITH 'Aprovação Cancelada'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CANCEL_AP_INSTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_cancel_ap_instal TABLES p_row_no TYPE lvc_t_roid.

  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL AND <fs_saida>-status <> 'P'.

    IF <fs_saida>-cod_alt IS NOT INITIAL.

      REFRESH: t_zppt_loc_instal.
      SELECT * FROM zppt_loc_instal
        INTO TABLE @DATA(t_zppt_loc_instal)
        WHERE cod_alt = @<fs_saida>-cod_alt.

      IF sy-subrc EQ 0.
        SELECT *
               FROM zppt_controle
               INTO TABLE t_zppt_controle
               WHERE werks EQ <fs_saida>-werks.
      ENDIF.
    ENDIF.

    LOOP AT t_zppt_loc_instal ASSIGNING FIELD-SYMBOL(<fs_zppt_loc_instal>).

      READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_loc_instal>-werks
                                                                     aprovador = sy-uname.

      IF sy-subrc IS INITIAL.
        <fs_zppt_loc_instal>-status       = 'P'.
        CLEAR: <fs_zppt_loc_instal>-aprovador, <fs_zppt_loc_instal>-dt_aprovacao, <fs_zppt_loc_instal>-hr_aprovacao.
      ELSE.
        MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF t_zppt_loc_instal[] IS NOT INITIAL.
    MODIFY zppt_loc_instal FROM TABLE t_zppt_loc_instal.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CANCEL_AP_MANUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_cancel_ap_manut TABLES p_row_no TYPE lvc_t_roid.

  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL.

    IF <fs_saida>-status(1) EQ 'A' OR <fs_saida>-status(1) EQ 'R'.
      IF <fs_saida>-werks IS NOT INITIAL AND <fs_saida>-warpl IS NOT INITIAL.

        REFRESH: t_zppt_plano_manu.
        SELECT * FROM zppt_plano_manu
          INTO TABLE @DATA(t_zppt_plano_manu)
          WHERE cod_alt = @<fs_saida>-cod_alt
            AND werks   = @<fs_saida>-werks
            AND warpl   = @<fs_saida>-warpl.

        IF sy-subrc EQ 0.
          SELECT *
                 FROM zppt_controle
                 INTO TABLE t_zppt_controle
                 WHERE werks EQ <fs_saida>-werks.
        ENDIF.
      ENDIF.

      LOOP AT t_zppt_plano_manu ASSIGNING FIELD-SYMBOL(<fs_zppt_plano_manu>).

        READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_plano_manu>-werks
                                                                       aprovador = sy-uname.

        IF sy-subrc IS INITIAL.
          <fs_zppt_plano_manu>-status  = 'P'.
          CLEAR: <fs_zppt_plano_manu>-aprovador, <fs_zppt_plano_manu>-dt_aprovacao, <fs_zppt_plano_manu>-hr_aprovacao.
        ELSE.
          MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE i000(zppr) WITH 'Efetue a aprovação para cancelar'.
    ENDIF.
  ENDLOOP.

  IF t_zppt_plano_manu[] IS NOT INITIAL.
    MODIFY zppt_plano_manu FROM TABLE t_zppt_plano_manu.
    COMMIT WORK.
    MESSAGE s000(zppr) WITH 'Aprovação Cancelada'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_REENVIO_EMAIL_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_reenvio_email_equip TABLES p_row_no TYPE lvc_t_roid.

  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL.

    IF <fs_saida>-werks IS NOT INITIAL AND <fs_saida>-equnr IS NOT INITIAL.
      SELECT * FROM zppt_inter_equip
        INTO TABLE @DATA(t_lista)
        WHERE cod_alt = @<fs_saida>-cod_alt
          AND werks   = @<fs_saida>-werks
          AND equnr   = @<fs_saida>-equnr.
    ENDIF.

*     Email cadastrados para controle
    SELECT SINGLE * FROM zppt_controle
      INTO @DATA(wl_email)
      WHERE werks = @<fs_saida>-werks
        AND aprovador = @sy-uname.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado' DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      IF <fs_saida>-status(1) EQ 'R'.
        v_ucomm = 'REJ'.
      ENDIF.

      PERFORM zf_email_equip TABLES t_lista USING wl_email.
      LOOP AT t_lista ASSIGNING FIELD-SYMBOL(<fs_lista>).
        <fs_lista>-status_email = 'S'.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF t_lista[] IS NOT INITIAL.
    MODIFY zppt_inter_equip FROM TABLE t_lista.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_REENVIO_EMAIL_INSTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_reenvio_email_instal TABLES p_row_no TYPE lvc_t_roid.
***    DATA: w_row_no     TYPE lvc_s_roid.
***
***    LOOP AT p_row_no INTO w_row_no.
***
***      CLEAR w_saida.
***      READ TABLE  t_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.
***
***      CHECK sy-subrc IS INITIAL.
***
***      IF <fs_saida>-cod_alt IS NOT INITIAL.
***        SELECT * FROM zppt_loc_instal
***          INTO TABLE @DATA(t_lista)
***          WHERE cod_alt = @<fs_saida>-cod_alt.
***      ENDIF.
***
****     Email cadastrados para controle
***      SELECT SINGLE * FROM zppt_controle
***        INTO @DATA(wl_email)
***        WHERE werks = @<fs_saida>-werks
***          AND aprovador = @<fs_saida>-aprovador.
***
***      IF sy-subrc IS NOT INITIAL.
***        MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado' DISPLAY LIKE 'E'.
***        RETURN.
***      ELSE.
***        PERFORM zf_email_instal TABLES t_lista USING wl_email.
***        LOOP AT t_lista ASSIGNING FIELD-SYMBOL(<fs_lista>).
***          <fs_lista>-status_email = 'S'.
***        ENDLOOP.
***      ENDIF.
***    ENDLOOP.
***
***    IF t_lista[] IS NOT INITIAL.
***      MODIFY zppt_loc_instal FROM TABLE t_lista.
***      COMMIT WORK.
***    ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_REENVIO_EMAIL_MANUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_ROW_NO  text
*----------------------------------------------------------------------*
FORM zf_reenvio_email_manut TABLES p_row_no TYPE lvc_t_roid.
  DATA: w_row_no     TYPE lvc_s_roid.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    CHECK sy-subrc IS INITIAL.

    IF <fs_saida>-werks IS NOT INITIAL AND <fs_saida>-warpl IS NOT INITIAL.
      SELECT * FROM zppt_plano_manu
        INTO TABLE @DATA(t_lista)
        WHERE cod_alt = @<fs_saida>-cod_alt
          AND werks   = @<fs_saida>-werks
          AND warpl   = @<fs_saida>-warpl.
    ENDIF.

*     Email cadastrados para controle
    SELECT SINGLE * FROM zppt_controle
      INTO @DATA(wl_email)
      WHERE werks = @<fs_saida>-werks
        AND aprovador = @sy-uname.

    IF sy-subrc IS NOT INITIAL.
      MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado' DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      IF <fs_saida>-status(1) EQ 'R'.
        v_ucomm = 'REJ'.
      ENDIF.

      PERFORM zf_email_manut TABLES t_lista USING wl_email.
      LOOP AT t_lista ASSIGNING FIELD-SYMBOL(<fs_lista>).
        <fs_lista>-status_email = 'S'.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  IF t_lista[] IS NOT INITIAL.
    MODIFY zppt_plano_manu FROM TABLE t_lista.
    COMMIT WORK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CAD_EMAIL_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_cad_email_equip .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,

        go_alv       TYPE REF TO cl_salv_table,
        go_functions TYPE REF TO cl_salv_functions,
        go_display   TYPE REF TO cl_salv_display_settings.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    IF sy-subrc IS INITIAL.
      SELECT SINGLE eqtyp
             INTO @DATA(lv_eqtyp)
             FROM equi
             WHERE equnr EQ @<fs_saida>-equnr.

      REFRESH: t_zppt_controle.
      SELECT * FROM zppt_controle
        INTO TABLE t_zppt_controle
        WHERE werks = <fs_saida>-werks
          AND eqtyp = lv_eqtyp.

      IF t_zppt_controle[] IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = go_alv CHANGING t_table = t_zppt_controle ).

*          go_functions = go_alv->get_functions( ).
*          go_functions->set_all( abap_true ).

        go_display = go_alv->get_display_settings( ).
        go_display->set_list_header( 'Aprovadores cadastrados' ).

        IF go_alv IS BOUND.
          go_alv->display( ).
        ENDIF.
      ELSE.
        MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado para Centro' <fs_saida>-werks.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CAD_EMAIL_INSTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_cad_email_instal .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,

        go_alv       TYPE REF TO cl_salv_table,
        go_functions TYPE REF TO cl_salv_functions,
        go_display   TYPE REF TO cl_salv_display_settings.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    IF sy-subrc IS INITIAL.

      REFRESH: t_zppt_controle.
      SELECT * FROM zppt_controle
        INTO TABLE t_zppt_controle
        WHERE werks = <fs_saida>-werks.

      IF t_zppt_controle[] IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = go_alv CHANGING t_table = t_zppt_controle ).

*          go_functions = go_alv->get_functions( ).
*          go_functions->set_all( abap_true ).

        go_display = go_alv->get_display_settings( ).
        go_display->set_list_header( 'Aprovadores cadastros' ).

        IF go_alv IS BOUND.
          go_alv->display( ).
        ENDIF.
      ELSE.
        MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado para Centro' <fs_saida>-werks.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CAD_EMAIL_MANUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_cad_email_manut .

  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        w_row_no     LIKE LINE OF t_row_no,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,

        go_alv       TYPE REF TO cl_salv_table,
        go_functions TYPE REF TO cl_salv_functions,
        go_display   TYPE REF TO cl_salv_display_settings.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LOOP AT t_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    IF sy-subrc IS INITIAL.

      SELECT SINGLE mptyp
             INTO @DATA(vl_mptyp)
             FROM mpla
             WHERE warpl EQ @<fs_saida>-warpl.

      REFRESH: t_zppt_controle.
      SELECT * FROM zppt_controle
        INTO TABLE t_zppt_controle
        WHERE werks = <fs_saida>-werks
          AND mptyp = vl_mptyp.

      IF t_zppt_controle[] IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = go_alv CHANGING t_table = t_zppt_controle ).

*          go_functions = go_alv->get_functions( ).
*          go_functions->set_all( abap_true ).

        go_display = go_alv->get_display_settings( ).
        go_display->set_list_header( 'Aprovadores cadastrados' ).

        IF go_alv IS BOUND.
          go_alv->display( ).
        ENDIF.
      ELSE.
        MESSAGE s000(zppr) WITH 'Nenhum aprovador cadastrado para Centro' <fs_saida>-werks.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EMAIL_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_email_equip TABLES p_lista STRUCTURE zppt_inter_equip
                    USING p_mail TYPE zppt_controle.

  TYPES: BEGIN OF y_itens.
           INCLUDE STRUCTURE zppt_inter_equip.
  TYPES:   maktx           TYPE makt-maktx,
           stlnr_txt       TYPE makt-maktx,
           aprovador_txt   TYPE v_username-name_text,
           colaborador_txt TYPE v_username-name_text,
         END OF y_itens.

  DATA: tl_itens TYPE TABLE OF y_itens.

  DATA: wl_itens LIKE LINE OF tl_itens.

  LOOP AT p_lista INTO DATA(wl_lista).
    MOVE-CORRESPONDING wl_lista TO wl_itens.

*     Busca nome do Colaborador
    IF  wl_itens-aenam IS NOT INITIAL.
      SELECT SINGLE name_text FROM v_username
        INTO (wl_itens-colaborador_txt)
         WHERE bname = wl_itens-aenam.
    ENDIF.

    APPEND wl_itens TO tl_itens.
    CLEAR wl_itens.
  ENDLOOP.

*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

*******************Prepare Corpo do E-mail*******************************************
  wl_objtxt-line = '<BR>'.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  READ TABLE tl_itens INTO wl_itens INDEX 1.

  IF v_ucomm EQ 'REJ'.
    wl_objtxt-line = 'Rejeitado as alterações dos dados mestres - Equipamento. '.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

    CONCATENATE 'Alteração rejeitada pelo ' sy-uname ' ,favor reverter alterações.'
          INTO wl_objtxt-line SEPARATED BY space.

  ELSE.
    CONCATENATE 'Alteração ' wl_itens-cod_alt ' pendente de aprovação.'
          INTO wl_objtxt-line SEPARATED BY space.
  ENDIF.

  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  MOVE '<TABLE BORDER=1>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col1
  MOVE '<TR style="background-color: #96A5AA;"> <TH> Centro </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col2
  MOVE '<TH> Equipamento </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
  MOVE '<TH> Descrição Campo Alterado </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
  MOVE '<TH> Conteúdo antigo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
  MOVE '<TH> Novo conteúdo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
  MOVE '<TH> Responsável modificação </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
  MOVE '<TH> Data da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
  MOVE '<TH> Hora da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  LOOP AT tl_itens INTO wl_itens.

*col1
    IF wl_itens-werks IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' wl_itens-werks '</TD>' INTO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ELSE.
      MOVE '<TR> <TD></TD>' TO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ENDIF.

*col2
    data(lv_equnr) = |{ wl_itens-equnr ALPHA = OUT }| .       "FF 12.02.24 - USER STORY 78112

    CONCATENATE '<TD>' lv_equnr '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
    CONCATENATE '<TD>' wl_itens-field '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-value_old '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
    CONCATENATE '<TD>' wl_itens-value_new '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
    CONCATENATE '<TD>' wl_itens-aenam '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
    WRITE  wl_itens-aedat TO lv_aedat USING EDIT MASK '__/__/____'.
    CONCATENATE '<TD>' lv_aedat '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
    WRITE  wl_itens-hr_alteracao TO lv_hr_alteracao USING EDIT MASK '__:__:__'.
    CONCATENATE '<TD>' lv_hr_alteracao '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  ENDLOOP.

  MOVE '</TABLE>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.  CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  CONCATENATE '<TABLE><TH>*** Este é um e-mail gerado automaticamente pelo sistema' sy-sysid sy-mandt ', não responder ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO wl_objtxt-line SEPARATED BY space.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

**********************Cabeçalhodo Email**********************************************

  MOVE 'Aprovar alteração de listas técnicas'(015) TO wl_docdata-obj_descr.
  wl_docdata-sensitivty = 'F'.

  DESCRIBE TABLE tl_objtxt LINES lv_lines.
  IF NOT lv_lines IS INITIAL.
    wl_docdata-doc_size = lv_lines * 255.
  ENDIF.

  CLEAR wl_objpack.
  wl_objpack-transf_bin = ''.
  wl_objpack-head_start = 1.
  wl_objpack-head_num = 0.
  wl_objpack-body_start = 1.
  wl_objpack-body_num = lv_lines.
  wl_objpack-doc_type = 'HTM'.
  wl_objpack-doc_size = ( lv_lines - 1 ) * 255 + strlen( wl_objtxt ).
  APPEND wl_objpack TO tl_objpack.

  IF p_mail IS NOT INITIAL.
    wl_receiver-receiver = p_mail-email.
    wl_receiver-rec_type = 'U'.
    wl_receiver-com_type = c_comtyp_int.
    APPEND wl_receiver TO tl_receiver.
    CLEAR wl_receiver.
  ENDIF.

  CHECK tl_receiver[] IS NOT INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wl_docdata
*     put_in_outbox              = c_x
*     commit_work                = 'X'
    IMPORTING
      sent_to_all                = lv_sentall
      new_object_id              = lv_nobjid
    TABLES
      packing_list               = tl_objpack[]
*     OBJECT_HEADER              =
*     CONTENTS_BIN               = tl_contents
      contents_txt               = tl_objtxt[]
*     CONTENTS_HEX               = tl_contents_hex
      receivers                  = tl_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.


  CLEAR: tl_receiver[], tl_objtxt[], tl_objpack[], lv_nobjid, lv_sentall, wl_docdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EMAIL_MANUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_email_manut TABLES p_lista STRUCTURE zppt_plano_manu
                     USING p_mail TYPE zppt_controle.


  TYPES: BEGIN OF y_itens.
           INCLUDE STRUCTURE zppt_plano_manu.
  TYPES:   maktx           TYPE makt-maktx,
           stlnr_txt       TYPE makt-maktx,
           aprovador_txt   TYPE v_username-name_text,
           colaborador_txt TYPE v_username-name_text,
         END OF y_itens.

  DATA: tl_itens TYPE TABLE OF y_itens.

  DATA: wl_itens LIKE LINE OF tl_itens.

  LOOP AT p_lista INTO DATA(wl_lista).
    MOVE-CORRESPONDING wl_lista TO wl_itens.

*     Busca nome do Colaborador
    IF  wl_itens-aenam IS NOT INITIAL.
      SELECT SINGLE name_text FROM v_username
        INTO (wl_itens-colaborador_txt)
         WHERE bname = wl_itens-aenam.
    ENDIF.

    APPEND wl_itens TO tl_itens.
    CLEAR wl_itens.
  ENDLOOP.

*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

*******************Prepare Corpo do E-mail*******************************************
  wl_objtxt-line = '<BR>'.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  READ TABLE tl_itens INTO wl_itens INDEX 1.

  IF v_ucomm EQ 'REJ'.
    wl_objtxt-line = 'Rejeitado as alterações dos dados mestres - Equipamento. '.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

    CONCATENATE 'Alteração rejeitada pelo ' sy-uname ' ,favor reverter alterações.'
          INTO wl_objtxt-line SEPARATED BY space.

  ELSE.
    CONCATENATE 'Alteração ' wl_itens-cod_alt ' pendente de aprovação.'
          INTO wl_objtxt-line SEPARATED BY space.
  ENDIF.

  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  MOVE '<TABLE BORDER=1>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col1
  MOVE '<TR style="background-color: #96A5AA;"> <TH> Centro </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col2
  MOVE '<TH> Plano </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
  MOVE '<TH> Descrição Campo Alterado </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
  MOVE '<TH> Conteúdo antigo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
  MOVE '<TH> Novo conteúdo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
  MOVE '<TH> Responsável modificação </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
  MOVE '<TH> Data da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
  MOVE '<TH> Hora da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  LOOP AT tl_itens INTO wl_itens.
*col1
    IF wl_itens-werks IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' wl_itens-werks '</TD>' INTO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ELSE.
      MOVE '<TR> <TD></TD>' TO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ENDIF.

*col2
    data(lv_warpl) = |{ wl_itens-warpl ALPHA = OUT }| .       "FF 12.02.24 - USER STORY 78112

    CONCATENATE '<TD>' lv_warpl '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
    CONCATENATE '<TD>' wl_itens-field '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-value_old '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
    CONCATENATE '<TD>' wl_itens-value_new '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
    CONCATENATE '<TD>' wl_itens-aenam '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
    WRITE  wl_itens-aedat TO lv_aedat USING EDIT MASK '__/__/____'.
    CONCATENATE '<TD>' lv_aedat '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
    WRITE  wl_itens-hr_alteracao TO lv_hr_alteracao USING EDIT MASK '__:__:__'.
    CONCATENATE '<TD>' lv_hr_alteracao '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  ENDLOOP.

  MOVE '</TABLE>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.  CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  CONCATENATE '<TABLE><TH>*** Este é um e-mail gerado automaticamente pelo sistema' sy-sysid sy-mandt ', não responder ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO wl_objtxt-line SEPARATED BY space.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

**********************Cabeçalhodo Email**********************************************

  MOVE 'Aprovar alteração de listas técnicas'(015) TO wl_docdata-obj_descr.
  wl_docdata-sensitivty = 'F'.

  DESCRIBE TABLE tl_objtxt LINES lv_lines.
  IF NOT lv_lines IS INITIAL.
    wl_docdata-doc_size = lv_lines * 255.
  ENDIF.

  CLEAR wl_objpack.
  wl_objpack-transf_bin = ''.
  wl_objpack-head_start = 1.
  wl_objpack-head_num = 0.
  wl_objpack-body_start = 1.
  wl_objpack-body_num = lv_lines.
  wl_objpack-doc_type = 'HTM'.
  wl_objpack-doc_size = ( lv_lines - 1 ) * 255 + strlen( wl_objtxt ).
  APPEND wl_objpack TO tl_objpack.

  IF p_mail IS NOT INITIAL.
    wl_receiver-receiver = p_mail-email.
    wl_receiver-rec_type = 'U'.
    wl_receiver-com_type = c_comtyp_int.
    APPEND wl_receiver TO tl_receiver.
    CLEAR wl_receiver.
  ENDIF.

  CHECK tl_receiver[] IS NOT INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wl_docdata
*     put_in_outbox              = c_x
*     commit_work                = 'X'
    IMPORTING
      sent_to_all                = lv_sentall
      new_object_id              = lv_nobjid
    TABLES
      packing_list               = tl_objpack[]
*     OBJECT_HEADER              =
*     CONTENTS_BIN               = tl_contents
      contents_txt               = tl_objtxt[]
*     CONTENTS_HEX               = tl_contents_hex
      receivers                  = tl_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  CLEAR: tl_receiver[], tl_objtxt[], tl_objpack[], lv_nobjid, lv_sentall, wl_docdata.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_EMAIL_INSTAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_email_instal TABLES p_lista STRUCTURE zppt_loc_instal
                     USING p_mail TYPE zppt_controle.

  TYPES: BEGIN OF y_itens.
           INCLUDE STRUCTURE zppt_loc_instal.
  TYPES:   maktx           TYPE makt-maktx,
           stlnr_txt       TYPE makt-maktx,
           aprovador_txt   TYPE v_username-name_text,
           colaborador_txt TYPE v_username-name_text,
         END OF y_itens.

  DATA: tl_itens TYPE TABLE OF y_itens.

  DATA: wl_itens LIKE LINE OF tl_itens.

  LOOP AT p_lista INTO DATA(wl_lista).
    MOVE-CORRESPONDING wl_lista TO wl_itens.

*     Busca nome do Colaborador
    IF  wl_itens-aenam IS NOT INITIAL.
      SELECT SINGLE name_text FROM v_username
        INTO (wl_itens-colaborador_txt)
         WHERE bname = wl_itens-aenam.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------
* Monta corpo do E-mail
*----------------------------------------------------------------------

*******************Prepare Corpo do E-mail*******************************************
  wl_objtxt-line = '<BR>'.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  READ TABLE tl_itens INTO wl_itens INDEX 1.

  CONCATENATE 'Alteração ' wl_itens-cod_alt ' pendente de aprovação.'
        INTO wl_objtxt-line SEPARATED BY space.

  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  MOVE '<TABLE BORDER=1>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.


  MOVE '<TR style="background-color: #96A5AA;"> <TH> Centro </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col1
  MOVE '<TH> Centro </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col2
  MOVE '<TH> Local </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
  MOVE '<TH> Denominação </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
  MOVE '<TH> Novo conteúdo do campo </TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
  MOVE '<TH> Conteúdo antigo do campo </TH>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
  MOVE '<TH>Responsável modificação</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
  MOVE '<TH> Data da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
  MOVE '<TH> Hora da modif.</TH> ' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  LOOP AT tl_itens INTO wl_itens.

*col1
    IF wl_itens-werks IS NOT INITIAL.
      CONCATENATE '<TR> <TD>' wl_itens-werks '</TD>' INTO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ELSE.
      MOVE '<TR> <TD></TD>' TO wl_objtxt-line.
      APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.
    ENDIF.

*col2
    data(lv_tlpnr) = |{ wl_itens-tplnr ALPHA = OUT }| .       "FF 12.02.24 - USER STORY 78112
    CONCATENATE '<TD>' lv_tlpnr '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col3
    CONCATENATE '<TD>' wl_itens-field '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col4
    CONCATENATE '<TD>' wl_itens-value_new '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col5
    CONCATENATE '<TD>' wl_itens-value_old '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col6
    CONCATENATE '<TD>' wl_itens-aenam '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col7
    WRITE  wl_itens-aedat TO lv_aedat USING EDIT MASK '__/__/____'.
    CONCATENATE '<TD>' lv_aedat '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

*col8
    WRITE  wl_itens-hr_alteracao TO lv_hr_alteracao USING EDIT MASK '__:__:__'.
    CONCATENATE '<TD>' lv_hr_alteracao '</TD>' INTO wl_objtxt-line.
    APPEND wl_objtxt TO tl_objtxt.   CLEAR wl_objtxt.

  ENDLOOP.

  MOVE '</TABLE>' TO wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt.  CLEAR wl_objtxt.

  MOVE '<BR><BR>' TO  wl_objtxt-line.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

  CONCATENATE '<TABLE><TH>*** Este é um e-mail gerado automaticamente pelo sistema' sy-sysid sy-mandt ', não responder ***</TH><TH></TH><TH></TH><TH></TH><TH></TH></TABLE>' INTO wl_objtxt-line SEPARATED BY space.
  APPEND wl_objtxt TO tl_objtxt. CLEAR wl_objtxt.

**********************Cabeçalhodo Email**********************************************

  MOVE 'Aprovar alteração de listas técnicas'(015) TO wl_docdata-obj_descr.
  wl_docdata-sensitivty = 'F'.

  DESCRIBE TABLE tl_objtxt LINES lv_lines.
  IF NOT lv_lines IS INITIAL.
    wl_docdata-doc_size = lv_lines * 255.
  ENDIF.

  CLEAR wl_objpack.
  wl_objpack-transf_bin = ''.
  wl_objpack-head_start = 1.
  wl_objpack-head_num = 0.
  wl_objpack-body_start = 1.
  wl_objpack-body_num = lv_lines.
  wl_objpack-doc_type = 'HTM'.
  wl_objpack-doc_size = ( lv_lines - 1 ) * 255 + strlen( wl_objtxt ).
  APPEND wl_objpack TO tl_objpack.

  IF p_mail IS NOT INITIAL.
    wl_receiver-receiver = p_mail-email.
    wl_receiver-rec_type = 'U'.
    wl_receiver-com_type = c_comtyp_int.
    APPEND wl_receiver TO tl_receiver.
    CLEAR wl_receiver.
  ENDIF.

  CHECK tl_receiver[] IS NOT INITIAL.

  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = wl_docdata
*     put_in_outbox              = c_x
*     commit_work                = 'X'
    IMPORTING
      sent_to_all                = lv_sentall
      new_object_id              = lv_nobjid
    TABLES
      packing_list               = tl_objpack[]
*     OBJECT_HEADER              =
*     CONTENTS_BIN               = tl_contents
      contents_txt               = tl_objtxt[]
*     CONTENTS_HEX               = tl_contents_hex
      receivers                  = tl_receiver[]
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SCREEN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_screen .
  CASE abap_true.
    WHEN r_ie03.
      LOOP AT SCREEN.
        IF screen-group1 = 'MD1'.
          screen-active = '1'.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 = 'MD2'.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 = 'MD3'.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
***      WHEN r_il03.
***        LOOP AT SCREEN.
***          IF screen-group1 = 'MD1'.
***            screen-active = '0'.
***            MODIFY SCREEN.
***          ENDIF.
***          IF screen-group1 = 'MD2'.
***            screen-active = '1'.
***            MODIFY SCREEN.
***          ENDIF.
***          IF screen-group1 = 'MD3'.
***            screen-active = '0'.
***            MODIFY SCREEN.
***          ENDIF.
***          MODIFY SCREEN.
***        ENDLOOP.
    WHEN r_ip03.
      LOOP AT SCREEN.
        IF screen-group1 = 'MD1'.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 = 'MD2'.
          screen-active = '0'.
          MODIFY SCREEN.
        ENDIF.
        IF screen-group1 = 'MD3'.
          screen-active = '1'.
          MODIFY SCREEN.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_REJEITAR_MODIFIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_rejeitar_modific .
  DATA: t_index_rows TYPE lvc_t_row,                        "#EC NEEDED
        t_row_no     TYPE lvc_t_roid,
        t_delete     TYPE TABLE OF zcot0013,

        l_linhas     TYPE sy-tabix,
        l_answer     TYPE c,

        w_0025       TYPE zppt0025.

  REFRESH: t_zppt_inter_equip,t_zppt_plano_manu,t_zppt_loc_instal.

  CALL METHOD v_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_index_rows
      et_row_no     = t_row_no.

  DESCRIBE TABLE t_row_no LINES l_linhas.
  IF l_linhas = 0.
    "Selecionar uma linha.
    MESSAGE i000(zppr) WITH 'Selecionar uma linha'(t03) DISPLAY LIKE 'E'.
    RETURN.
  ELSEIF l_linhas > 1.
    "Selecionar apenas uma linha
    MESSAGE i000(zppr) WITH 'Selecionar apenas uma linha'(t04) DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE abap_true.
    WHEN r_ie03."Equipamento
      PERFORM zf_rejeita_equip TABLES t_row_no.
***      WHEN r_il03."Instalação
***        PERFORM zf_rejeita_instal TABLES t_row_no.
    WHEN r_ip03."Planos Manutenção
      PERFORM zf_rejeita_manut TABLES t_row_no.
  ENDCASE.

  PERFORM zf_buscar_objetos_modificados.
  PERFORM zf_preparar_saida.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_REJEITA_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_rejeita_equip  TABLES p_row_no TYPE lvc_t_roid.
  DATA: w_row_no TYPE lvc_s_roid,
        t_lista  TYPE TABLE OF zppt_inter_equip.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    IF <fs_saida>-status(1) NE 'P'.
      MESSAGE s000(zppr) WITH 'Registro não está Pendente para Rejeitar'.
    ENDIF.

    CHECK sy-subrc IS INITIAL AND <fs_saida>-status(1) EQ 'P'.

    IF <fs_saida>-werks IS NOT INITIAL AND <fs_saida>-equnr IS NOT INITIAL.

      REFRESH: t_zppt_inter_equip.
      SELECT * FROM zppt_inter_equip
        INTO TABLE @DATA(t_zppt_inter_equip)
        WHERE cod_alt = @<fs_saida>-cod_alt
          AND werks   = @<fs_saida>-werks
          AND equnr   = @<fs_saida>-equnr.

      IF sy-subrc EQ 0.
        SELECT *
               FROM zppt_controle
               INTO TABLE t_zppt_controle
               WHERE werks EQ <fs_saida>-werks.
      ENDIF.
    ENDIF.

    LOOP AT t_zppt_inter_equip ASSIGNING FIELD-SYMBOL(<fs_zppt_inter_equip>).

      READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_inter_equip>-werks
                                                                     aprovador = sy-uname.

      IF sy-subrc IS INITIAL.
        <fs_zppt_inter_equip>-status       = 'R'.
***          <fs_zppt_inter_equip>-aprovador    = sy-uname.
***          <fs_zppt_inter_equip>-dt_aprovacao = sy-datum.
***          <fs_zppt_inter_equip>-hr_aprovacao = sy-uzeit.

        APPEND <fs_zppt_inter_equip> TO t_lista.

        SELECT SINGLE adr6~smtp_addr
               INTO @DATA(lv_email)
               FROM usr21
               INNER JOIN adr6 ON adr6~addrnumber EQ usr21~addrnumber
                              AND adr6~persnumber EQ usr21~persnumber
               WHERE usr21~bname EQ  @<fs_zppt_inter_equip>-aenam.

        IF lv_email IS NOT INITIAL.
          w_zppt_controle-email = lv_email.
          PERFORM zf_email_equip TABLES t_lista USING w_zppt_controle.
          CLEAR: t_lista[].
        ELSE.
          MESSAGE s000(zppr) WITH 'Usuário ' <fs_zppt_inter_equip>-aenam ' sem email cadastrado.'  DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ELSE.
        MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF t_zppt_inter_equip[] IS NOT INITIAL.
    MODIFY zppt_inter_equip FROM TABLE t_zppt_inter_equip.
    COMMIT WORK.
    MESSAGE s000(zppr) WITH 'Registro rejeitado'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_REJEITA_MANUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_rejeita_manut  TABLES p_row_no TYPE lvc_t_roid.

  DATA: w_row_no TYPE lvc_s_roid,
        t_lista  TYPE TABLE OF zppt_plano_manu.

  LOOP AT p_row_no INTO w_row_no.

    CLEAR w_saida.
    READ TABLE  t_saida1 ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX w_row_no-row_id.

    IF <fs_saida>-status(1) NE 'P'.
      MESSAGE s000(zppr) WITH 'Registro não está Pendente para Rejeitar'.
    ENDIF.

    CHECK sy-subrc IS INITIAL AND <fs_saida>-status(1) EQ 'P'.

    IF <fs_saida>-werks IS NOT INITIAL AND <fs_saida>-warpl IS NOT INITIAL.

      REFRESH: t_zppt_plano_manu.
      SELECT * FROM zppt_plano_manu
        INTO TABLE @DATA(t_zppt_plano_manu)
        WHERE cod_alt = @<fs_saida>-cod_alt
          AND werks   = @<fs_saida>-werks
          AND warpl   = @<fs_saida>-warpl.

      IF sy-subrc EQ 0.
        SELECT *
               FROM zppt_controle
               INTO TABLE t_zppt_controle
               WHERE werks EQ <fs_saida>-werks.
      ENDIF.
    ENDIF.

    LOOP AT t_zppt_plano_manu ASSIGNING FIELD-SYMBOL(<fs_zppt_plano_manu>).

      READ TABLE t_zppt_controle INTO DATA(w_zppt_controle) WITH KEY werks = <fs_zppt_plano_manu>-werks
                                                                     aprovador = sy-uname.

      IF sy-subrc IS INITIAL.

        <fs_zppt_plano_manu>-status       = 'R'.
***          <fs_zppt_plano_manu>-aprovador    = sy-uname.
***          <fs_zppt_plano_manu>-dt_aprovacao = sy-datum.
***          <fs_zppt_plano_manu>-hr_aprovacao = sy-uzeit.

        APPEND <fs_zppt_plano_manu> TO t_lista.

        SELECT SINGLE adr6~smtp_addr
               INTO @DATA(lv_email)
               FROM usr21
               INNER JOIN adr6 ON adr6~addrnumber EQ usr21~addrnumber
                              AND adr6~persnumber EQ usr21~persnumber
               WHERE usr21~bname EQ  @<fs_zppt_plano_manu>-aenam.

        IF lv_email IS NOT INITIAL.
          w_zppt_controle-email = lv_email.
          PERFORM zf_email_manut TABLES t_lista USING w_zppt_controle.
          CLEAR: t_lista[].
        ELSE.
          MESSAGE s000(zppr) WITH 'Usuário ' <fs_zppt_plano_manu>-aenam ' sem email cadastrado.'  DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.
      ELSE.
        MESSAGE s000(zppr) WITH 'Usuário não cadastrado como aprovador ' 'Transação ZPM0099' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF t_zppt_plano_manu[] IS NOT INITIAL.
    MODIFY zppt_plano_manu FROM TABLE t_zppt_plano_manu.
    COMMIT WORK.
    MESSAGE s000(zppr) WITH 'Registro rejeitado'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CLEAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zf_clear .
  CLEAR: t_zppt_inter_equip[],t_zppt_plano_manu[],t_zppt_loc_instal[],t_zppt_controle[],
         t_dd07t[],t_saida[],t_saida1[],t_saida2[],t_coltab2[],r_status[],
         w_disvariant,w_layout,w_saida,w_saida1,w_saida2,v_info,v_ucomm,
         tl_objtxt[],tl_objpack[],tl_receiver[],wl_docdata,wl_objtxt,wl_objpack,
         wl_receiver,lv_bukrs,lv_rec_cnt,lv_lines,lv_sentall,lv_nobjid,
         lv_aedat,lv_hr_alteracao,lv_qtd_antiga,lv_qtd_atual.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ZF_ZPPR024
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM zf_atualiza_tabelas.

  SUBMIT zppr024 AND RETURN. ""Programa que atualiza os dados nas tabelas zppt_plano_manu e zppt_inter_equip"Programa que atualiza os dados nas tabelas zppt_plano_manu e zppt_inter_equip

ENDFORM.
