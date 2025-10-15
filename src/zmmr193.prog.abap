*&--------------------------------------------------------------------&*
*&                         Consultoria                                &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: RODRIGO C.                                              &*
*& Data.....: 13/09/2023                                              &*
*& Descrição: Relatório Integração SAP MRPxCOUPA                      &*
*& Transação: ZMM0217                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*
REPORT zmmr190.


*----------------------------------------------------------------------*
* Tabelas                                                          *
*----------------------------------------------------------------------*
TABLES: lfa1, bkpf, sscrfields.


*----------------------------------------------------------------------*
* Tipo de Tabelas                                                          *
*----------------------------------------------------------------------*

"Variáveis
DATA: gva_bukrs TYPE bukrs.
DATA: icon_form_bloq TYPE string.
DATA: icon_form_desbloq TYPE string.
DATA: icon_form_status TYPE string.

TYPES:

  BEGIN OF ty_dados_integracao,
    zvalida         TYPE char30,                   "Status processamento
    lifnr           TYPE zmmt0172-lifnr,           "Fornecedor
    zid_formulario  TYPE zmmt0172-zid_formulario,  "Código formulario
    id_integracao   TYPE zmmt0172-id_integracao,   "Id integração
    zid_msg_01      TYPE zmmt0172-zid_msg_01,      "Código mensagem 01
    zid_msg_02      TYPE zmmt0172-zid_msg_02,      "Código mensagem 02
    zid_msg_03      TYPE zmmt0172-zid_msg_03,      "Código mensagem 03
    zmsg_justif     TYPE zmmt0172-zmsg_justif,     "Justificativa
    ztipo_bloq      TYPE zmmt0172-ztipo_bloq,      "Tipo de bloqueio
    us_criacao      TYPE zmmt0172-us_criacao,      "Usuario executante
    dt_criacao      TYPE zmmt0172-dt_criacao,      "Data processamento
    hr_criacao      TYPE zmmt0172-hr_criacao,      "Hr do processamento
    log_proc        TYPE zmmt0172-log_proc,        "Logs de processamento
    ztipo_proc      TYPE char30,                   "Tipo processamento
    zid_approvals   TYPE zmmt0172-zid_approvals,   "Código aprovador formulario
    log_proc_status TYPE zmmt0172-log_proc_status, "Status processamento
  END   OF ty_dados_integracao.


*----------------------------------------------------------------------*
* Tabela interna                                                       *
*----------------------------------------------------------------------*
DATA: t_zmmt0172         TYPE TABLE OF zmmt0172,
      t_dados_integracao TYPE TABLE OF ty_dados_integracao.


*----------------------------------------------------------------------*
* Work Área                                                            *
*----------------------------------------------------------------------*
DATA: wa_zmmt0172         TYPE zmmt0172,
      wa_dados_integracao TYPE ty_dados_integracao.


*----------------------------------------------------------------------*
* Objetos                                                             *
*----------------------------------------------------------------------*
DATA: gob_custom_container        TYPE REF TO cl_gui_custom_container,
      gob_dd_document             TYPE REF TO cl_dd_document,
      gob_splitter_container_main TYPE REF TO cl_gui_splitter_container,
      gob_splitter_container_topo TYPE REF TO cl_gui_splitter_container,

      gob_gui_container_topo      TYPE REF TO cl_gui_container,
      gob_gui_container_filtro    TYPE REF TO cl_gui_container,
      gob_gui_container_logo      TYPE REF TO cl_gui_container,
      gob_gui_container_grid      TYPE REF TO cl_gui_container,
      gob_gui_picture             TYPE REF TO cl_gui_picture,
      git_fcat                    TYPE lvc_t_fcat,
      gob_gui_alv_grid            TYPE REF TO cl_gui_alv_grid,
      lines                       TYPE sy-tabix,
      wa_selected_rows            TYPE lvc_s_row,
      it_selected_rows            TYPE lvc_t_row.



"Contantes
CONSTANTS: gco_p1 TYPE char05 VALUE '0001'. "Importar formulario de bloqueio.
CONSTANTS: gco_p2 TYPE char05 VALUE '0002'. "Importar formulario de desbloqueio.
CONSTANTS: gco_p3 TYPE char05 VALUE '0003'. "Envia status de desbloqueio/desbloqueio.


*----------------------------------------------------------------------*
* Tela de seleção                                                             *
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr, "Nº conta do fornecedor
                  s_us_c  FOR bkpf-sname, "Usuário Executor
                  s_id_f  FOR lfa1-lifnr. "Código de identificação do formulario
SELECTION-SCREEN: END OF BLOCK b1.


*----------------------------------------------------------------------*
* INITIALIZATION                                                             *
*----------------------------------------------------------------------*
SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.

INITIALIZATION.

  icon_form_bloq = icon_oo_interface && 'Importar formulario de bloqueio'.
  sscrfields-functxt_01 = icon_form_bloq.

  icon_form_desbloq = icon_oo_interface && 'Importar formulario de desbloqueio'.
  sscrfields-functxt_02 = icon_form_desbloq.

  icon_form_status = icon_oo_interface && 'Enviar status bloq/desbl fornecedor'.
  sscrfields-functxt_03 = icon_form_status.

AT SELECTION-SCREEN. "PAI
  CASE sscrfields-ucomm. "pushbutton pressed
    WHEN 'FC01'.
      PERFORM fm_importa_formulario_bloqueio.

    WHEN 'FC02'.
      PERFORM fm_importa_form_desbloqueio.

   WHEN 'FC03'.
      PERFORM fm_atualiza_status_fornecedor.
  ENDCASE.


*----------------------------------------------------------------------*
* START-OF-SELECTION                                                             *
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM fm_start_of_selection.

END-OF-SELECTION.
  PERFORM fm_end_of_selection.
*&---------------------------------------------------------------------*
*&      Form  FM_START_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_start_of_selection .

  PERFORM fm_dados_seleciona.
  PERFORM fm_dados_processa.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_SELECIONA
*&---------------------------------------------------------------------*
FORM fm_dados_seleciona .

  IF NOT s_id_f[] IS INITIAL.
    LOOP AT s_id_f.
      PACK s_id_f-low TO s_id_f-low.
      CONDENSE s_id_f-low.
      MODIFY s_id_f INDEX sy-tabix.
    ENDLOOP.
  ENDIF.

  " Seleciona Dados formulario enviado MRP Coupa
  SELECT *
         FROM zmmt0172
         INTO TABLE t_zmmt0172
         WHERE lifnr          IN s_lifnr
         AND   zid_formulario IN s_id_f
         AND   us_criacao     IN s_us_c.
  IF sy-subrc NE 0.
    MESSAGE i001(zcarga) WITH 'Dados não encontrados na tabela ZMMT0172'.
    STOP.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_DADOS_PROCESSA
*&---------------------------------------------------------------------*
FORM fm_dados_processa .


  LOOP AT t_zmmt0172 INTO wa_zmmt0172.

    "( Adcionar uma badeira verde se ZVALIDA = 'X', senão adicionar uma bandeira vermelha no status processamento ).
    IF   wa_zmmt0172-zvalida EQ abap_true.
      MOVE icon_green_light TO wa_dados_integracao-zvalida.
    ELSEIF wa_zmmt0172-zvalida EQ abap_false.
      MOVE icon_red_light   TO wa_dados_integracao-zvalida.
    ENDIF.
    wa_dados_integracao-lifnr           = wa_zmmt0172-lifnr.
    wa_dados_integracao-zid_formulario  = wa_zmmt0172-zid_formulario.
    wa_dados_integracao-id_integracao   = wa_zmmt0172-id_integracao.
    wa_dados_integracao-zid_msg_01      = wa_zmmt0172-zid_msg_01.
    wa_dados_integracao-zid_msg_02      = wa_zmmt0172-zid_msg_02.
    wa_dados_integracao-zid_msg_03      = wa_zmmt0172-zid_msg_03.
    wa_dados_integracao-zmsg_justif     = wa_zmmt0172-zmsg_justif.
    wa_dados_integracao-ztipo_bloq      = wa_zmmt0172-ztipo_bloq.
    wa_dados_integracao-us_criacao      = wa_zmmt0172-us_criacao.
    wa_dados_integracao-dt_criacao      = wa_zmmt0172-dt_criacao.
    wa_dados_integracao-hr_criacao      = wa_zmmt0172-hr_criacao.
    wa_dados_integracao-log_proc        = wa_zmmt0172-log_proc.

    "(se ZTIPO_PROC  = '01' = Bloqueio fornecedor se for diferente adiconar o teste desbloqueio fornecedor ).
    IF   wa_zmmt0172-ztipo_proc   EQ '1'.
      wa_dados_integracao-ztipo_proc = 'Bloqueio fornecedor'.
    ELSEIF wa_zmmt0172-ztipo_proc NE '1'.
      wa_dados_integracao-ztipo_proc = 'desbloqueio fornecedor'.
    ENDIF.

    wa_dados_integracao-zid_approvals   = wa_zmmt0172-zid_approvals.
    wa_dados_integracao-log_proc_status = wa_zmmt0172-log_proc_status.

    APPEND wa_dados_integracao TO t_dados_integracao.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_END_OF_SELECTION
*&---------------------------------------------------------------------*
FORM fm_end_of_selection .
  IF NOT t_dados_integracao[] IS INITIAL.
    CALL SCREEN 0100.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'PF-0100'.
  SET TITLEBAR  'TB-0100' WITH 'Relatório Integração SAP MRPxCOUPA'.

  PERFORM fm_criar_objetos.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIAR_OBJETOS
*&---------------------------------------------------------------------*
FORM fm_criar_objetos .

  DATA: lva_data(22) TYPE c,
        w_layout     TYPE lvc_s_layo.

  DATA: gs_variant  TYPE disvariant.
  gs_variant-report      = sy-repid.

  PERFORM fm_cria_fieldcat.


  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
    EXPORTING
       i_titulo  = 'Relatório Integração SAP MRPxCOUPA'
       i_filtros = VALUE zif_screen_linha_filtro_t( ( parametro = 'Data Posição' valor = lva_data ) )
     CHANGING
       alv = gob_gui_alv_grid
     )
     EQ abap_true.


*    CREATE OBJECT event_receiver.
*    SET HANDLER event_receiver->hotspot_click  FOR gob_gui_alv_grid.
*    SET HANDLER event_receiver->get_ucomm  FOR gob_gui_alv_grid.

    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = 'X'.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.



    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        is_variant                    = gs_variant
      CHANGING
        it_outtab                     = t_dados_integracao
        it_fieldcatalog               = git_fcat
*       IT_SORT                       =
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CRIA_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_cria_fieldcat .

  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  git_fcat = VALUE lit_fieldcat_aux(

( fieldname = 'zvalida'
  coltext   = 'Status Processamento'
  col_opt   = 'X'
  icon      = 'X'
  no_zero   = '' )


( fieldname = 'lifnr'
  coltext   = 'Fornecedor'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'zid_formulario'
  coltext   = 'Id. Formulário'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'id_integracao'
  coltext   = 'Id integração'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'zid_msg_01'
  coltext   = 'Código mensagem 01'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'zid_msg_02'
  coltext   = 'Código mensagem 02'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'zid_msg_03'
  coltext   = 'Código mensagem 03'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'zmsg_justif'
  coltext   = 'Justificativa'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'ztipo_bloq'
  coltext   = 'Tipo de bloqueio'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'us_criacao'
  coltext   = 'Usuario executante'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'dt_criacao'
  coltext   = 'Data processamento'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'hr_criacao'
  coltext   = 'Hr do processamento'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'log_proc'
  coltext   = 'Logs de processamento'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'ztipo_proc'
  coltext   = 'Tipo de processamento'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'zid_approvals'
  coltext   = 'aprovador formulario'
  col_opt   = 'X'
  no_zero   = '' )

( fieldname = 'log_proc_status'
  coltext   = 'Log processamento'
  col_opt   = 'X'
  no_zero   = '' )

).
*  DATA: lc_col_pos  TYPE lvc_colpos.
*  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.
*  CLEAR: git_fcat.
*
*  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
*    EXPORTING
*      i_structure_name = 'ZDE_DATA_COMBOIO_ABASTECIMENTO'
*    CHANGING
*      ct_fieldcat      = git_fcat.
*
*  LOOP AT git_fcat ASSIGNING <fs_cat>.
*
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form fm_importa_formulario_bloqueio
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_importa_formulario_bloqueio .
  "Processar programa.
  SUBMIT zmmr186 AND RETURN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_importa_form_desbloqueio
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_importa_form_desbloqueio .
  "Processar programa.
  SUBMIT zmmr187 AND RETURN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form fm_importa_form_status
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fm_atualiza_status_fornecedor .
"Processar programa.
  SUBMIT ZMMR0037 AND RETURN.
ENDFORM.
