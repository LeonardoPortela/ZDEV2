*&---------------------------------------------------------------------*
*& Report  ZCOR008
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zcor008.

TABLES: zcot0003.

DATA: cl_alv TYPE REF TO cl_gui_alv_grid.
TYPE-POOLS: slis.
DATA: fieldcatalog TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_tab_group TYPE slis_t_sp_group_alv,
      gd_layout    TYPE slis_layout_alv,
      gd_repid     LIKE sy-repid.
DATA: sort      TYPE slis_t_sortinfo_alv WITH HEADER LINE.

DATA: BEGIN OF t_003 OCCURS 0.
        INCLUDE STRUCTURE zcot0003.
DATA: END OF t_003.

*&---------------------------------------------------------------------*
*& Tela de Seleção                                                     *
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_tknum FOR zcot0003-tknum,
                s_fknum FOR zcot0003-fknum,
                s_ebeln FOR zcot0003-ebeln,
                s_belnr FOR zcot0003-belnr,
                s_data  FOR zcot0003-dt_atual,
                s_hora  FOR zcot0003-hora_atul,
                s_user  FOR zcot0003-usnam.
SELECTION-SCREEN END OF BLOCK b1.

*&---------------------------------------------------------------------*
*& Início do Programa                                                  *
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  PERFORM: seleciona_dados,

           chama_alv.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
FORM seleciona_dados .

  SELECT * FROM zcot0003 INTO TABLE t_003
    WHERE tknum     IN s_tknum AND
          fknum     IN s_fknum AND
          ebeln     IN s_ebeln AND
          belnr     IN s_belnr AND
          dt_atual  IN s_data  AND
          hora_atul IN s_hora  AND
          usnam     IN s_user.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  CHAMA_ALV
*&---------------------------------------------------------------------*
FORM chama_alv .

*  DATA: BEGIN OF t_field OCCURS 0.
*          INCLUDE STRUCTURE alvt_t_fcat_wp.
*  DATA: END OF t_field.

DATa: T_FIELD LIKE alv_s_fcat_wp occurs 0 WITH HEADER LINE.

  CALL FUNCTION 'ECP_SRV_FIELDCAT_MERGE'
    EXPORTING
      i_structure_name       = 'ZCOT0003'
      i_client_never_display = 'X'
    TABLES
      ct_fieldcat            = t_field
    EXCEPTIONS
      structure_not_found    = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CLEAR fieldcatalog. REFRESH fieldcatalog.
  LOOP AT t_field.
    MOVE-CORRESPONDING t_field TO fieldcatalog.
    APPEND fieldcatalog.
    CLEAR fieldcatalog.
  ENDLOOP.

  gd_layout-no_input          = 'X'.
  gd_layout-zebra             = 'X'.
  gd_layout-colwidth_optimize = 'X'.
  gd_layout-box_tabname       = 'T_003'.
  gd_layout-window_titlebar   = 'Apropriação de Custo de Frete'.
  gd_layout-detail_titlebar   = 'Apropriação de Custo de Frete'.

  gd_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = gd_repid
*      i_callback_user_command  = 'COMANDO'
*      i_callback_pf_status_set = 'PF_STATUS'
      is_layout                = gd_layout
      it_fieldcat              = fieldcatalog[]
      it_sort                  = sort[]
      i_save                   = 'A'
    TABLES
      t_outtab                 = t_003
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.

ENDFORM.                    " CHAMA_ALV
