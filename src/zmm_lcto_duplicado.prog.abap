*&---------------------------------------------------------------------*
*& Report  ZMMR0030
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 14/04/2015                                              &*
*& Descrição: Interface TRACECOTTON x SAP PP - IN                     &*
*& Transação: PP                                                      &*
*& Request..: DEVK945561                                              &*
*&--------------------------------------------------------------------&*
REPORT  zmm_lcto_duplicado  MESSAGE-ID ztracecotton.
TYPE-POOLS vrm.

TABLES: zppt0002.

SELECTION-SCREEN BEGIN OF BLOCK b1.
  SELECT-OPTIONS : s_cotton FOR zppt0002-id_cotton.
SELECTION-SCREEN END OF BLOCK b1.

**----------------------------------------------------------------------
** VARIEVEIS
**----------------------------------------------------------------------
TYPES: BEGIN OF ty_charg,
         charg TYPE mseg-charg,
         quant TYPE i.
TYPES: END   OF ty_charg.

**----------------------------------------------------------------------
** VARIEVEIS
**----------------------------------------------------------------------
DATA: t_sele       TYPE TABLE OF mseg,
      w_sele       TYPE mseg,
      t_mseg       TYPE TABLE OF mseg,
      w_mseg       TYPE mseg,
      t_mseg2      TYPE TABLE OF mseg,
      w_mseg2      TYPE mseg,
      t_charg      TYPE TABLE OF ty_charg,
      w_charg      TYPE ty_charg,
      t_det        TYPE TABLE OF mseg,
      w_det        TYPE mseg,
      l_grid_title TYPE lvc_title,
      l_program    TYPE sy-repid,
      w_layout     TYPE slis_layout_alv,
      t_fieldcat   TYPE slis_t_fieldcat_alv.

*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM f_seleciona_dados.

  IF t_mseg[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Não foram selecionados dados!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM f_processa_dados.
  PERFORM f_exibe_envio.

*&---------------------------------------------------------------------*
*& selecao dados
*&---------------------------------------------------------------------*
FORM f_seleciona_dados.

  SELECT *
    INTO TABLE t_mseg
    FROM mseg
   WHERE xblnr_mkpf IN s_cotton.

  CHECK t_mseg[] IS NOT INITIAL.

  SELECT *
    INTO TABLE t_mseg2
    FROM mseg
     FOR ALL ENTRIES IN t_mseg
   WHERE charg = t_mseg-charg.

ENDFORM.

*&---------------------------------------------------------------------*
*& processar dados
*&---------------------------------------------------------------------*
FORM f_processa_dados.

  FREE: t_det.

  t_sele[] = t_mseg[].

  SORT t_sele BY xblnr_mkpf.
  DELETE ADJACENT DUPLICATES FROM t_sele COMPARING xblnr_mkpf.

  LOOP AT t_sele INTO w_sele.

    FREE: t_charg.

    LOOP AT t_mseg INTO w_mseg WHERE xblnr_mkpf = w_sele-xblnr_mkpf.
      LOOP AT t_mseg2   INTO w_mseg2 WHERE charg = w_mseg-charg.
        w_charg-charg      = w_mseg2-charg.
        w_charg-quant      = 1.
        COLLECT w_charg INTO t_charg.
      ENDLOOP.
    ENDLOOP.

    SORT t_charg BY quant.
    READ TABLE t_charg INTO w_charg INDEX 1.

    LOOP AT t_mseg INTO w_mseg WHERE charg = w_charg-charg.
      MOVE w_mseg    TO w_det.
      APPEND w_det   TO t_det.
    ENDLOOP.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*& processar dados
*&---------------------------------------------------------------------*
FORM f_exibe_envio.

  l_program                  = sy-repid.
  l_grid_title               = 'Possiveis documentos que devem ser Estornados'.
  w_layout-expand_all        = abap_true.
  w_layout-colwidth_optimize = abap_true.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = l_program
      i_structure_name       = 'MSEG'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = l_program
      is_layout          = w_layout
      it_fieldcat        = t_fieldcat
      i_grid_title       = l_grid_title
      i_save             = 'A'
*     i_screen_start_column = 10
*     i_screen_start_line   = 02
*     i_screen_end_column   = 182
*     i_screen_end_line  = 20
    TABLES
      t_outtab           = t_det
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
