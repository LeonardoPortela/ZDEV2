*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : Relatório Vendas Frames                                 *
* Descrição  : Conferência de Pedágio                                  *
* Módulo     : SD                                Transação: ZSDT0001   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 17/10/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdr0003 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TYPE-POOLS icon.

TABLES: vbap    ,
        kna1    ,
        vbak    ,
        mara    ,
        zsdt0020.
*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
DATA: BEGIN OF type_saida.
INCLUDE TYPE zsdt0020.
DATA:  pre_fix TYPE zsdt0020-qtd_fix_premio,
       chi_fix TYPE zsdt0020-qtd_fix_chicago,
       icon    TYPE icon-name               ,
       werks   TYPE char30                  .
DATA: END   OF type_saida.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_zsdt0020 LIKE TABLE OF type_saida,
      t_vbap     TYPE TABLE OF vbap      ,
      t_t001w    TYPE TABLE OF t001w     ,
      t_zsdt0021 TYPE TABLE OF zsdt0021  ,
      t_saida    TYPE TABLE OF zsdt0021  ,
      t_sum      LIKE TABLE OF type_saida,
      t_fcat     TYPE TABLE OF lvc_s_fcat,
      t_fcat_2   TYPE TABLE OF lvc_s_fcat.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_cont   TYPE REF TO cl_gui_custom_container,
      s_alv    TYPE REF TO cl_gui_alv_grid        ,
      s_layout TYPE lvc_s_layo                    ,
      v_maktx  TYPE makt-maktx                    ,
      s_cont_2 TYPE REF TO cl_gui_custom_container,
      s_alv_2  TYPE REF TO cl_gui_alv_grid        .

*----------------------------------------------------------------------*
*                               CONSTANTES                             *
*----------------------------------------------------------------------*
CONSTANTS: c_table  TYPE char10 VALUE 'T_ZSDT0020',
           c_table1 TYPE char10 VALUE 'T_ZSDT0021'.

**----------------------------------------------------------------------*
**                               Classes                                *
**----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION DEFERRED.
DATA s_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION                            *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS
       zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
         IMPORTING
          e_row_id e_column_id es_row_no.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION                        *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD zm_handle_hotspot.
*   Click Hotspot
    PERFORM z_handle_hotspot USING e_row_id
                                   e_column_id
                                   es_row_no.
  ENDMETHOD.                    "zm_handle_toolbar


ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
  s_werks FOR vbap-werks,
  s_charg FOR vbap-charg,
  s_kunnr FOR kna1-kunnr,
  s_vbeln FOR vbak-vbeln,
  s_perio FOR zsdt0020-periodo OBLIGATORY,
  s_matnr FOR mara-matnr NO INTERVALS NO-EXTENSION
                               OBLIGATORY,
  s_dats  FOR zsdt0020-dt_fix_ptax NO INTERVALS
                                   NO-EXTENSION,
  s_stat  FOR zsdt0020-status NO INTERVALS
                              NO-EXTENSION.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-002.
PARAMETERS:
  p_conso TYPE char1 RADIOBUTTON GROUP rb1 USER-COMMAND radio
                                           DEFAULT 'X',
  p_anali TYPE char1 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END   OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK c1 WITH FRAME TITLE text-044.
PARAMETERS:
  p_varia TYPE slis_vari.
SELECTION-SCREEN END   OF BLOCK c1.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                          AT SELECTION-SCREEN                         *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.
* Get Variante
  PERFORM z_get_variante.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Seleciona Dados
  PERFORM: z_seleciona_dados,
* Processa Dados
           z_processa_dados ,
* Monta FieldCat
           z_monta_fieldcat .

  IF p_anali IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    CALL SCREEN 0200.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS                                        *
*&---------------------------------------------------------------------*
*                            Seleciona Dados                           *
*----------------------------------------------------------------------*
FORM z_seleciona_dados.

* Seleciona Tabela ZSDT0020
  PERFORM: z_seleciona_zsdt0020,
* Seleciona VBAP
           z_seleciona_vbap    ,
* Seleciona T001W
           z_seleciona_t001w   ,
* Seleciona Tabela ZSDT0021
           z_seleciona_zsdt0021.

ENDFORM.                    " Z_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA_DADOS                                         *
*&---------------------------------------------------------------------*
*                             Processa Dados                           *
*----------------------------------------------------------------------*
FORM z_processa_dados.

  DATA: sl_sum      LIKE LINE OF t_sum,
        sl_zsdt0020 LIKE LINE OF t_sum,
        sl_vbap     TYPE vbap         ,
        sl_t001w    TYPE t001w        ,
        vl_index    TYPE i            .

  REFRESH t_sum.

  LOOP AT t_zsdt0020 INTO sl_zsdt0020.

    vl_index  = sy-tabix.

    READ TABLE t_vbap INTO sl_vbap
      WITH KEY vbeln = sl_zsdt0020-vbeln
      BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      READ TABLE t_t001w INTO sl_t001w
        WITH KEY werks = sl_vbap-werks
        BINARY SEARCH.
      CONCATENATE sl_vbap-werks
                  sl_t001w-name1
             INTO sl_zsdt0020-werks SEPARATED BY '-'.
      MODIFY t_zsdt0020 FROM sl_zsdt0020
        INDEX vl_index TRANSPORTING werks.
    ELSE.
      DELETE t_zsdt0020 INDEX vl_index.
    ENDIF.

    CLEAR: sl_zsdt0020,
           sl_t001w   ,
           sl_vbap    .

  ENDLOOP.

  IF t_zsdt0020[] IS INITIAL.
    MESSAGE i836 WITH text-016.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF p_anali IS INITIAL.

    LOOP AT t_zsdt0020 INTO sl_zsdt0020.
      sl_sum-vbeln           = sl_zsdt0020-vbeln.
      sl_sum-periodo         = sl_zsdt0020-periodo.
      sl_sum-qtd_fix_premio  = sl_zsdt0020-qtd_fix_premio.
      sl_sum-qtd_fix_chicago = sl_zsdt0020-qtd_fix_chicago.
      COLLECT sl_sum INTO t_sum.
      CLEAR: sl_zsdt0020,
             sl_sum     .
    ENDLOOP.

    DELETE ADJACENT DUPLICATES FROM t_zsdt0020 COMPARING vbeln periodo.

  ENDIF.

  LOOP AT t_zsdt0020 INTO sl_zsdt0020.
    vl_index = sy-tabix.
    READ TABLE t_sum INTO sl_sum
      WITH KEY vbeln   = sl_zsdt0020-vbeln
               periodo = sl_zsdt0020-periodo
      BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      sl_zsdt0020-qtd_fix_premio  = sl_sum-qtd_fix_premio.
      sl_zsdt0020-qtd_fix_chicago = sl_sum-qtd_fix_chicago.
      sl_zsdt0020-pre_fix         = sl_zsdt0020-qtd_mes - sl_zsdt0020-qtd_fix_premio.
      sl_zsdt0020-chi_fix         = sl_zsdt0020-qtd_mes - sl_zsdt0020-qtd_fix_chicago.
    ENDIF.
    IF sl_zsdt0020-status EQ 2.
      sl_zsdt0020-icon = icon_green_light.
    ELSE.
      sl_zsdt0020-icon = icon_yellow_light.
    ENDIF.
    MODIFY t_zsdt0020 FROM sl_zsdt0020 INDEX vl_index.
    CLEAR: sl_zsdt0020,
           sl_sum     .
  ENDLOOP.

ENDFORM.                    " Z_PROCESSA_DADOS

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_FIELDCAT                                         *
*&---------------------------------------------------------------------*
*                           Monta FieldCat                             *
*----------------------------------------------------------------------*
FORM z_monta_fieldcat.

  REFRESH: t_fcat  ,
           t_fcat_2.

  CHECK NOT t_zsdt0020[] IS INITIAL.

  IF p_anali IS INITIAL.
*   Preenche FieldCat
    PERFORM z_preenche_fieldcat USING:
      'ICON'            text-015 06 space,
      'CLIENTE'         text-003 07 space,
      'NAME1'           text-004 20 space,
      'VBELN'           text-005 10 space,
      'QTD_CONT'        text-006 18 space,
      'WERKS'           text-043 30 space,
      'PERIODO'         text-007 07 space,
      'QTD_MES'         text-008 18 space,
      'QTD_FIX_PREMIO'  text-009 16 space,
      'PRE_FIX'         text-010 16 space,
      'QTD_FIX_CHICAGO' text-011 16 space,
      'CHI_FIX'         text-012 16 space.

  ELSE.
*   Preenche FieldCat
    PERFORM z_preenche_fieldcat USING:
      'ICON'            text-015 06 'X'  ,
      'CLIENTE'         text-003 07 space,
      'NAME1'           text-004 20 space,
      'VBELN'           text-005 10 space,
      'QTD_CONT'        text-006 18 space,
      'REFERENCIA'      text-017 10 space,
      'WERKS'           text-043 30 space,
      'PERIODO'         text-007 07 space,
      'QTD_MES'         text-008 18 space,
      'DT_PREMIO'       text-018 10 space,
      'NIV_PREMIO'      text-019 16 space,
      'QTD_FIX_PREMIO'  text-009 16 space,
      'DT_CHICAGO'      text-020 11 space,
      'QTD_FIX_CHICAGO' text-011 16 space,
      'NIV_CHICAGO'     text-021 16 space,
      'SPREAD'          text-022 16 space,
      'PTAX_MEDIO'      text-023 16 space,
      'DT_FIX_PTAX'     text-024 16 space,
      'FRETE_FOB'       text-025 16 space,
      'MOEDA_FOB'       text-026 09 space,
      'FRETE_CIF'       text-027 13 space,
      'CUSTO_FINANC'    text-028 13 space,
      'COMISSAO'        text-029 10 space,
      'PIS'             text-030 10 space,
      'COFINS'          text-031 10 space,
      'ICMS'            text-032 10 space.

*   Preenche FieldCat 2
    PERFORM z_preenche_fieldcat_2 USING:
      'DTA_PRECO'       text-033 10 space,
      'MED_PREMIO'      text-034 13 space,
      'MED_CHICAGO'     text-035 13 space,
      'SPREAD'          text-036 13 space,
      'CUSTO_IMP'       text-037 15 space,
      'FRETE_FOB'       text-038 13 space,
      'FRETE_CIF'       text-039 13 space,
      'PTAX'            text-040 13 space,
      'PRECO_FINAL'     text-041 13 space,
      'ORDEM_VENDA'     text-042 10 space.

  ENDIF.

* Monta Layout
  PERFORM z_layout.

ENDFORM.                    " Z_MONTA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0020                                     *
*&---------------------------------------------------------------------*
*                       Seleciona Tabela ZSDT0020                      *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0020.

  DATA vl_data TYPE dats.

  REFRESH t_zsdt0020.

  SELECT *
    FROM zsdt0020
    INTO TABLE t_zsdt0020
  WHERE  vbeln       IN s_vbeln
    AND  periodo     IN s_perio
    AND  cliente     IN s_kunnr
    AND  matnr       IN s_matnr
    AND  status      IN s_stat.
*    AND  dt_fix_ptax IN s_dats.

  IF NOT s_dats-low IS INITIAL.
    CONCATENATE s_perio-low(4)
                s_perio-low+4(2)
                '01'
           INTO vl_data.
    DELETE t_zsdt0020 WHERE dt_premio NE space
                        AND ( dt_premio GE s_dats-low
                         OR   dt_premio LE vl_data ).
    DELETE t_zsdt0020 WHERE dt_chicago NE space
                        AND ( dt_chicago GE s_dats-low
                         OR   dt_chicago LE vl_data ).
  ENDIF.

  SORT t_zsdt0020 BY vbeln   ASCENDING
                     periodo ASCENDING
                     id      ASCENDING.

  CHECK t_zsdt0020[] IS INITIAL.
  MESSAGE i836 WITH text-016.
  LEAVE LIST-PROCESSING.

ENDFORM.                    " Z_SELECIONA_ZSDT0020

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT                                      *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat                          *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat USING p_field   TYPE c
                               p_desc    TYPE c
                               p_len     TYPE n
                               p_hotspot TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = c_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.
  sl_fcat-hotspot   = p_hotspot.

  APPEND sl_fcat TO t_fcat.

ENDFORM.                    " Z_PREENCHE_FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT                                                 *
*&---------------------------------------------------------------------*
*                            Monta Layout                              *
*----------------------------------------------------------------------*
FORM z_layout.

  CLEAR s_layout.

  s_layout-zebra = 'X'.

ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100' OR
         '0200'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR  'TB0100'.
  ENDCASE.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_OBJ_ALV  OUTPUT                                     *
*&---------------------------------------------------------------------*
*                                 Obj Alv                              *
*----------------------------------------------------------------------*
MODULE zm_obj_alv OUTPUT.

* Instancia Container
  PERFORM: z_inst_cont ,
* Instancia Alv
           z_inst_alv  ,
* Instancia Eventos
           z_inst_event,
* Exibe Alv
           z_exibe_alv .

ENDMODULE.                 " ZM_OBJ_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_CONT                                              *
*&---------------------------------------------------------------------*
*                       Instancia Container                            *
*----------------------------------------------------------------------*
FORM z_inst_cont.

  IF s_cont IS INITIAL.

    CREATE OBJECT s_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE i836 WITH text-013.
    ENDIF.

  ENDIF.

  CHECK NOT p_anali IS INITIAL.

  IF s_cont_2 IS INITIAL.

    CREATE OBJECT s_cont_2
      EXPORTING
        container_name              = 'CC_ALV2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE i836 WITH text-013.
    ENDIF.

  ENDIF.

ENDFORM.                    " Z_INST_CONT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_ALV                                               *
*&---------------------------------------------------------------------*
*                              Instancia Alv                           *
*----------------------------------------------------------------------*
FORM z_inst_alv.

  IF s_alv IS INITIAL.

    CREATE OBJECT s_alv
      EXPORTING
        i_parent          = s_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE i836 WITH text-014.
    ENDIF.

  ENDIF.

  CHECK NOT p_anali IS INITIAL.

  IF s_alv_2 IS INITIAL.

    CREATE OBJECT s_alv_2
      EXPORTING
        i_parent          = s_cont_2
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE i836 WITH text-014.
    ENDIF.

  ENDIF.

ENDFORM.                    " Z_INST_ALV

*&---------------------------------------------------------------------*
*&      Form  Z_EXIBE_ALV                                              *
*&---------------------------------------------------------------------*
*                                Exibe Alv                             *
*----------------------------------------------------------------------*
FORM z_exibe_alv.

  DATA: vl_int     TYPE int4      ,
        vl_variant TYPE disvariant.

  vl_variant-report  = sy-repid.
  vl_variant-variant = p_varia.

  CALL METHOD s_alv->set_table_for_first_display
    EXPORTING
      i_save                        = 'X'
      i_default                     = 'X'
      is_layout                     = s_layout
      is_variant                    = vl_variant
    CHANGING
      it_outtab                     = t_zsdt0020
      it_fieldcatalog               = t_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT p_anali IS INITIAL.

  CALL METHOD s_alv_2->set_table_for_first_display
    EXPORTING
*      i_save                        =
      i_default                     = 'X'
      is_layout                     = s_layout
    CHANGING
      it_outtab                     = t_saida
      it_fieldcatalog               = t_fcat_2
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*  CALL METHOD s_alv->set_ready_for_input.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " Z_EXIBE_ALV

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100' OR
         '0200'.
      CASE sy-ucomm.
        WHEN 'BACK' OR
             'CANC' OR
             'EXIT'.
          LEAVE TO SCREEN 0.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT                                         *
*&---------------------------------------------------------------------*
*                              Click Hotspot                           *
*----------------------------------------------------------------------*
FORM z_handle_hotspot USING e_row_id    TYPE  lvc_s_row
                            e_column_id TYPE  lvc_s_col
                            es_row_no   TYPE  lvc_s_roid.

  CHECK 1 = 1.

  CASE e_column_id.
    WHEN 'ICON'.
*     Verifca STATUS
      PERFORM z_verifica_status USING e_row_id.
  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_INST_EVENT                                             *
*&---------------------------------------------------------------------*
*                           Instancia Eventos                          *
*----------------------------------------------------------------------*
FORM z_inst_event.

  CHECK s_event IS INITIAL AND
        p_conso IS INITIAL.

  CREATE OBJECT s_event.
  SET HANDLER s_event->zm_handle_hotspot FOR s_alv.

ENDFORM.                    " Z_INST_EVENT

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_STATUS                                        *
*&---------------------------------------------------------------------*
*                              Verifca STATUS                          *
*----------------------------------------------------------------------*
FORM z_verifica_status USING e_row_id TYPE lvc_s_row.

  DATA sl_zsdt0020 LIKE LINE OF t_zsdt0020.

  READ TABLE t_zsdt0020 INTO sl_zsdt0020
    INDEX e_row_id-index.

  REFRESH t_saida.

  IF sl_zsdt0020-status EQ 2.
    t_saida[] = t_zsdt0021[].
    DELETE t_saida WHERE vbeln NE sl_zsdt0020-vbeln.
  ENDIF.

  CALL METHOD s_alv_2->refresh_table_display.

ENDFORM.                    " Z_VERIFICA_STATUS

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_ZSDT0021                                     *
*&---------------------------------------------------------------------*
*                         Seleciona Tabela ZSDT0021                    *
*----------------------------------------------------------------------*
FORM z_seleciona_zsdt0021.

  DATA: tl_zsdt0020 LIKE TABLE OF type_saida ,
        sl_zsdt0020 LIKE LINE OF  tl_zsdt0020.

  REFRESH t_zsdt0021.
  CLEAR v_maktx.

  CHECK NOT t_zsdt0020[] IS INITIAL.
  tl_zsdt0020[] = t_zsdt0020[].
  SORT tl_zsdt0020 BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_zsdt0020 COMPARING vbeln.
  READ TABLE tl_zsdt0020 INTO sl_zsdt0020 INDEX 1.

  SELECT *
    FROM zsdt0021
    INTO TABLE t_zsdt0021
    FOR ALL ENTRIES IN tl_zsdt0020
  WHERE  vbeln EQ tl_zsdt0020-vbeln.

* Descrição do Material
  v_maktx = sl_zsdt0020-maktx.

ENDFORM.                    " Z_SELECIONA_ZSDT0021

*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_FIELDCAT_2                                    *
*&---------------------------------------------------------------------*
*                           Preenche FieldCat 2                        *
*----------------------------------------------------------------------*
FORM z_preenche_fieldcat_2 USING p_field   TYPE c
                                 p_desc    TYPE c
                                 p_len     TYPE n
                                 p_hotspot TYPE c.

  DATA sl_fcat TYPE lvc_s_fcat.

  sl_fcat-tabname   = c_table.
  sl_fcat-fieldname = p_field.
  sl_fcat-scrtext_l = p_desc.
  sl_fcat-scrtext_m = p_desc.
  sl_fcat-scrtext_s = p_desc.
  sl_fcat-outputlen = p_len.
  sl_fcat-hotspot   = p_hotspot.

  APPEND sl_fcat TO t_fcat_2.

ENDFORM.                    " Z_PREENCHE_FIELDCAT_2

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_VBAP                                         *
*&---------------------------------------------------------------------*
*                             Seleciona VBAP                           *
*----------------------------------------------------------------------*
FORM z_seleciona_vbap.

  DATA tl_zsdt0020 LIKE TABLE OF type_saida.

  REFRESH t_vbap.

  CHECK NOT t_zsdt0020[] IS INITIAL.
  tl_zsdt0020[] = t_zsdt0020[].
  SORT tl_zsdt0020 BY vbeln ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_zsdt0020 COMPARING vbeln.

  SELECT *
    FROM vbap
    INTO TABLE t_vbap
    FOR ALL ENTRIES IN tl_zsdt0020
  WHERE vbeln EQ tl_zsdt0020-vbeln
    AND werks IN s_werks
    AND charg IN s_charg.

  SORT t_vbap BY vbeln ASCENDING.

  CHECK t_vbap[] IS INITIAL.
  MESSAGE i836 WITH text-016.
  LEAVE LIST-PROCESSING.

ENDFORM.                    " Z_SELECIONA_VBAP

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_T001W                                        *
*&---------------------------------------------------------------------*
*                              Seleciona T001W                         *
*----------------------------------------------------------------------*
FORM z_seleciona_t001w.

  DATA tl_vbap TYPE TABLE OF vbap.

  REFRESH t_t001w.

  CHECK NOT t_vbap[] IS INITIAL.
  tl_vbap[] = tl_vbap[].
  SORT tl_vbap BY werks ASCENDING.
  DELETE ADJACENT DUPLICATES FROM tl_vbap COMPARING werks.

  SELECT *
    FROM t001w
    INTO TABLE t_t001w
    FOR ALL ENTRIES IN tl_vbap
  WHERE  werks EQ tl_vbap-werks.

  SORT t_t001w BY werks ASCENDING.

ENDFORM.                    " Z_SELECIONA_T001W

*&---------------------------------------------------------------------*
*&      Form  Z_GET_VARIANTE                                           *
*&---------------------------------------------------------------------*
*                            Get Variante                              *
*----------------------------------------------------------------------*
FORM z_get_variante.

  DATA: sl_variant TYPE disvariant,
        sl_out     TYPE disvariant.

  sl_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = sl_variant
    IMPORTING
      es_variant    = sl_out
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CHECK NOT sl_out-variant IS INITIAL.

  p_varia = sl_out-variant.

ENDFORM.                    " Z_GET_VARIANTE
