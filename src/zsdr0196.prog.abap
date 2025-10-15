**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Rubenilson - Reestruturação da transação ZSDT0123                    |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Wellington Silva                                                     |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Cadastro de Formação de Lote                                              |*
**/===========================================================================\*

REPORT zsdr0196.

***********************************************************************************************
* TABELAS
***********************************************************************************************
TABLES: zsdt0051.

***********************************************************************************************
* PARAMETROS DE SELEÇÃO
***********************************************************************************************
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_solov FOR zsdt0051-nro_sol_ov NO-EXTENSION NO INTERVALS OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b1.

***********************************************************************************************
* ESTRUTURAS
***********************************************************************************************
TYPES: BEGIN OF ty_0066.
TYPES: icon(4)       TYPE c.
TYPES: status_trace(4) TYPE c.
       INCLUDE STRUCTURE zsdt0066.
TYPES: color(4)        TYPE c,
       werks_desc      TYPE name1,
       matnr_desc      TYPE maktx,
       terminal_desc   TYPE name1,
       ponto_c_desc    TYPE name1,
       lentrega_desc   TYPE name1,
       kunnr_desc      TYPE name1,
       btgew           TYPE gsgew.

TYPES END OF ty_0066.

TYPES: BEGIN OF ty_0051.
         INCLUDE TYPE zsdt0051.
TYPES:   %nro_sol_ov TYPE zsdt0045-objek,
         %bstkd      TYPE zsdt0045-contrato.
TYPES END OF  ty_0051.

TYPES: t_0066 TYPE TABLE OF zsdt0066.
******************************************************************************************************
******** DECLARAÇÕES GLOBAIS
******************************************************************************************************
DATA: it_0066         TYPE TABLE OF ty_0066 WITH HEADER LINE,
      ws_0066         TYPE ty_0066,
      wa_0066         TYPE zsdt0066,
      wa_edit1        TYPE zsdt0066,
      it_0051         TYPE TABLE OF ty_0051 WITH HEADER LINE,
      it_new          TYPE TABLE OF ty_0066 WITH HEADER LINE,
      it_save         TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      wa_save         TYPE zsdt0066,
      it_edit         TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      it_edit1        TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      it_dele         TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      it_0066_old     TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      l_nro_sol_ov    TYPE objnum,               "*-CS2023000189-19.04.2023-#108709-JT
      l_tabix         TYPE sy-tabix,            "*-CS2023000189-19.04.2023-#108709-JT
      l_referencia    TYPE numc10   VALUE 1000, "*-CS2023000189-19.04.2023-#108709-JT
      l_bstkd         TYPE text50             , "*-CS2023000189-19.04.2023-#108709-JT
      l_back          TYPE char1,               "*-CS2023000189-19.04.2023-#108709-JT
      t_zsdt0328      TYPE TABLE OF zsdt0328,   "*-CS2023000189-19.04.2023-#108709-JT
      w_zsdt0328      TYPE zsdt0328,            "*-CS2023000189-19.04.2023-#108709-JT
      t_zsdt0327      TYPE TABLE OF zsdt0327,   "*-CS2023000189-19.04.2023-#108709-JT
      w_zsdt0327      TYPE zsdt0327,            "*-CS2023000189-19.04.2023-#108709-JT
      t_ret           TYPE TABLE OF zsdt0328,   "*-CS2023000189-19.04.2023-#108709-JT
      wa_0066_old     TYPE zsdt0066,
      it_alt_qtd      TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      tg_save_log     TYPE TABLE OF zsdt0083,
      it_log_old      TYPE STANDARD TABLE OF zsdt0066 WITH DEFAULT KEY,
      obj_alv         TYPE REF TO cl_gui_alv_grid,
      obj_cont        TYPE REF TO cl_gui_custom_container,
      wa_variant      TYPE disvariant,
      wa_layout       TYPE lvc_s_layo,
      wa_stable       TYPE lvc_s_stbl,
      str             TYPE REF TO data,
      l_value_edit    TYPE sy-tabix,
      it_fcat         TYPE lvc_t_fcat,
      it_fcat_log     TYPE lvc_t_fcat,
      tg_msg_ret      TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wg_mensagem(30),
      lv_acao         TYPE sy-ucomm,
      vlgort          TYPE t001l-lgort.

DATA: cont       TYPE n LENGTH 2,
      str_l(255) TYPE c,
      campo      TYPE char20,
      campo2     TYPE char20,
      tam        TYPE i.

FIELD-SYMBOLS: <fs_campo>  TYPE any,
               <fs_campo1> TYPE any,
               <fs_campo2> TYPE any.

CLASS zcl_events DEFINITION.
  PUBLIC SECTION.

    METHODS: on_click FOR EVENT hotspot_click OF cl_gui_alv_grid IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

DATA(obj_events) = NEW zcl_events( ).

INITIALIZATION.
  DATA(o_form_lote) = NEW zcl_cadastro_formacao_lote( ).

**********************************************************************************************
*SELECTION-SCREEN VALUE-REQUEST
**********************************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_solov-low.
  s_solov-low = o_form_lote->monta_matchcode( 'NRO_SOL_OV' ).
**********************************************************************************************

CLASS zcl_events IMPLEMENTATION.

  METHOD on_click.

    CASE e_column_id.
      WHEN 'VBELN'.
        CHECK it_0066[ e_row_id ]-vbeln IS NOT INITIAL.  "*-CS2023000189-19.04.2023-#108709-JT
        DATA(vbeln) = it_0066[ e_row_id ]-vbeln.         "*-CS2023000189-19.04.2023-#108709-JT

        SET PARAMETER ID 'AUN' FIELD vbeln.
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

*-CS2023000189-19.04.2023-#108709-JT-inicio
      WHEN 'NRO_SOL_OV' OR 'INSTRUCAO'.
        DATA(_nro_sol_ov) = it_0066[ e_row_id ]-nro_sol_ov.
        DATA(_posnr)      = it_0066[ e_row_id ]-posnr.
        l_nro_sol_ov      = _nro_sol_ov.
        l_referencia      = _posnr.
        l_bstkd           = it_0051-bstkd.

        CALL FUNCTION 'ZSD_SELECAO_FORMACAO_LOTE'
          EXPORTING
            i_nro_sol_ov    = l_nro_sol_ov
            i_posnr         = _posnr
            i_contrato      = l_bstkd
            i_referencia    = l_referencia
            i_somente_exibe = abap_true
            i_editar        = abap_true
          TABLES
            t_retorno       = t_zsdt0328.  "t_ret.

      WHEN 'STATUS_TRACE'.
        _nro_sol_ov  = it_0066[ e_row_id ]-nro_sol_ov.
        _posnr       = it_0066[ e_row_id ]-posnr.

        CALL FUNCTION 'ZSD_LOG_ENVIO_TRACE_COTTON'
          EXPORTING
            i_nro_sol_ov   = _nro_sol_ov
            i_posnr        = _posnr
            i_tipo_integra = 'OV'.
*-CS2023000189-19.04.2023-#108709-JT-fim


    ENDCASE.

  ENDMETHOD.

ENDCLASS.

***********************************************************************************************
* SELEÇÃO DE DADOS
***********************************************************************************************
START-OF-SELECTION.

  o_form_lote->bloqueia_nro_solicitacao( EXPORTING nro_solicitacao = s_solov-low ).

  TRY .

      o_form_lote->seleciona_dados_gerais( EXPORTING nro_solicitacao = s_solov-low
                                           IMPORTING header = it_new
                                                     t_0066 = it_0066[]
                                                     t_0327 = t_zsdt0327[]
                                                     t_0328 = t_zsdt0328[] ).

    CATCH zcl_cx_excecoes_cad_form_lote INTO DATA(lo_excecao).

      MESSAGE lo_excecao->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.

  ENDTRY.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TI0100'.

*-CS2023000189-19.04.2023-#108709-JT-inicio
* IF sy-ucomm <> 'BNT_EDIT'.
  LOOP AT SCREEN.
    IF screen-name = 'IT_NEW-VOLUM'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
* ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai_0100 INPUT.
  DATA: lv_msg TYPE char100.

  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO CURRENT TRANSACTION.
    WHEN 'CANCEL'.
      PERFORM limpa_variaveis.

      o_form_lote->seleciona_dados_gerais( EXPORTING nro_solicitacao = s_solov-low
                                           IMPORTING header = it_new
                                                     t_0066 = it_0066[]
                                                     t_0327 = t_zsdt0327[]
                                                     t_0328 = t_zsdt0328[] ).


    WHEN 'SAVE'.
      o_form_lote->save( EXPORTING nro_solicitacao = s_solov-low
                                   acao            = lv_acao
                                   t_fcat          = it_fcat
                         CHANGING header    = it_new
                                  t_0066    = it_0066[]
                                  t_0327    = t_zsdt0327[]
                                  t_0328    = t_zsdt0328[]
                                  t_alt_qtd = it_alt_qtd[]
                                  t_msg_ret = tg_msg_ret[] ).

      PERFORM limpa_variaveis.

      o_form_lote->seleciona_dados_gerais( EXPORTING nro_solicitacao = s_solov-low
                                           IMPORTING header = it_new
                                                     t_0066 = it_0066[]
                                                     t_0327 = t_zsdt0327[]
                                                     t_0328 = t_zsdt0328[] ).


    WHEN 'BNT_ADD'.
      lv_acao = sy-ucomm.
      o_form_lote->add_novo_lote( EXPORTING acao     = lv_acao
                                  IMPORTING texto_bt_dinamico = wg_mensagem
                                  CHANGING header    = it_new
                                           t_0066    = it_0066[]
                                           t_fcat    = it_fcat[]
                                           t_msg_ret = tg_msg_ret[] ).
    WHEN 'BNT_EDIT'.
      lv_acao = sy-ucomm.
      o_form_lote->edit_lote( EXPORTING obj_alv = obj_alv
                              CHANGING acao = lv_acao
                                       header = it_new
                                       t_0066 = it_0066[] ).
    WHEN 'BNT_DEL'.
      lv_acao = sy-ucomm.
      o_form_lote->deleta_lote( EXPORTING obj_alv = obj_alv
                                CHANGING acao = lv_acao
                                         t_0066 = it_0066[]
                                         t_dele = it_dele[] ).
    WHEN 'BNT_TRACE'.
      lv_acao = sy-ucomm.
      o_form_lote->reenvia_trace( EXPORTING obj_alv = obj_alv
                                  CHANGING t_0066 = it_0066[] ).
      o_form_lote->get_dados_trace( CHANGING t_0066 = it_0066[] ).
      CLEAR it_new.

    WHEN 'SHOW_MSGRE'.
      o_form_lote->exibe_menssagens( EXPORTING acao      = lv_acao
                                        t_0066    = it_0066[]
                                        t_fcat    = it_fcat[]
                              IMPORTING texto_bt_dinamico = wg_mensagem
                              CHANGING  header    = it_new
                                        t_msg_ret = tg_msg_Ret[] ).
    WHEN 'BNT_LTO'.
      lv_acao = sy-ucomm.
      o_form_lote->liberar_lote( EXPORTING nro_solicitacao = s_solov-low
                                           obj_alv = obj_alv
                                 IMPORTING mensagem = lv_msg
                                  CHANGING acao = lv_acao
                                           t_0066 = it_0066[] ).
      IF lv_msg IS NOT INITIAL.
        MESSAGE lv_msg TYPE 'S'.
      ENDIF.
    WHEN 'BNT_QTD'.
      lv_acao = sy-ucomm.
      o_form_lote->altera_qtd( EXPORTING t_0066 = it_0066[]
                               CHANGING acao = lv_acao
                                        header = it_new ).
    WHEN 'REFRESH'.
      PERFORM limpa_variaveis.

      o_form_lote->seleciona_dados_gerais( EXPORTING nro_solicitacao = s_solov-low
                                           IMPORTING header = it_new
                                                     t_0066 = it_0066[]
                                                     t_0327 = t_zsdt0327[]
                                                     t_0328 = t_zsdt0328[] ).

    WHEN 'BTNF4'.
      DATA(lv_seq) = o_form_lote->busca_instrucoes( EXPORTING acao = lv_acao
                                                              nro_solicitacao = s_solov-low
                                                              value_edit = l_value_edit
                                                    IMPORTING referencia = l_referencia
                                                    CHANGING header = it_new
                                                             t_0066 = it_0066[]
                                                             t_0328 = t_zsdt0328[]
                                                             t_alt_qtd = it_alt_qtd[] ).

      o_form_lote->get_0045( EXPORTING seq = lv_seq
                                       referencia = l_referencia
                                       t_0328 = t_zsdt0328[]
                             CHANGING  header = it_new ).

      o_form_lote->complementa_dados_header( CHANGING header = it_new ).

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CREATE_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE create_fcat OUTPUT.

  DATA: lt_string TYPE TABLE OF string.

  FREE it_fcat.

  ASSIGN 'TY_0066' TO FIELD-SYMBOL(<fs_str>).
  CREATE DATA str TYPE (<fs_str>).

  it_fcat = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( str ) ) ) ).

  DATA(lines_fcat) = lines( it_fcat[] ).

  LOOP AT it_fcat ASSIGNING FIELD-SYMBOL(<fcat>).

    IF <fcat>-fieldname EQ 'VBELN'.
      <fcat>-hotspot = abap_true.
    ENDIF.

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF <fcat>-fieldname EQ 'NRO_SOL_OV' OR
       <fcat>-fieldname EQ 'INSTRUCAO'.
      <fcat>-hotspot = abap_true.
    ENDIF.
    IF <fcat>-fieldname EQ 'STATUS_TRACE'.
      <fcat>-icon    = abap_true.
      <fcat>-hotspot = abap_true.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

    LOOP AT it_0066 ASSIGNING FIELD-SYMBOL(<f66>).

      campo = <fcat>-fieldname.
      ASSIGN COMPONENT campo OF STRUCTURE <f66> TO <fs_campo1>.

      WRITE <fs_campo1> TO str_l.
      IF strlen( str_l ) > tam.
        tam = strlen( str_l ).
      ENDIF.

    ENDLOOP.

    cont = 0.

    DO .
      IF lines_fcat < cont.
        EXIT.
      ENDIF.

      ADD 1 TO cont.
      campo = |TEXT-A{ cont }|.
      ASSIGN (campo) TO <fs_campo>.

      SPLIT <fs_campo> AT '#' INTO TABLE lt_string.

      IF lt_string[ 1 ] EQ <fcat>-fieldname.

        IF lt_string[ 1 ] EQ <fcat>-fieldname.
          <fcat>-scrtext_s = <fcat>-scrtext_m = <fcat>-scrtext_l = lt_string[ 2 ].
          <fcat>-col_pos = lt_string[ 3 ].
        ENDIF.

        IF <fcat>-inttype EQ 'P'.
          <fcat>-intlen = <fcat>-outputlen = lt_string[ 4 ].
        ELSE.
          IF tam IS INITIAL.
            <fcat>-intlen = <fcat>-outputlen = lt_string[ 4 ].
          ELSE.
            <fcat>-outputlen =  COND #( WHEN tam >= 200 THEN lt_string[ 4 ] ELSE tam  ).    "TAM  + 1.
            <fcat>-intlen = <fcat>-outputlen.
          ENDIF.

        ENDIF.

        <fcat>-no_out =
        SWITCH #( lt_string[ 5 ]
                                              WHEN abap_true THEN lt_string[ 5 ]
                                              ELSE abap_false ).
        CLEAR tam.
        EXIT.

      ENDIF.

    ENDDO.
  ENDLOOP.

  DELETE it_fcat WHERE col_pos = 0.

  CHECK obj_alv IS NOT INITIAL.

  CALL METHOD obj_alv->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fcat.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  POPULA_OBJ  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE popula_obj OUTPUT.

  TRY .
      o_form_lote->check_botao( CHANGING t_0066 = it_0066[] ).
      o_form_lote->bloq_desbloq_campos_tela( EXPORTING acao = lv_acao ).
    CATCH zcl_cx_excecoes_cad_form_lote INTO DATA(lo_excecao1).
      MESSAGE lo_excecao1->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

  IF obj_cont IS INITIAL.

    CREATE OBJECT obj_cont
      EXPORTING
        container_name = 'CC_FORMACAODELOTE'.

    CREATE OBJECT obj_alv
      EXPORTING
        i_shellstyle    = 0
        i_parent        = obj_cont
        i_appl_events   = abap_false
        i_fcat_complete = abap_false.

    o_form_lote->set_layout( CHANGING layout = wa_layout
                                      variant = wa_variant
                                      stable  = wa_stable ).

    CREATE OBJECT obj_events.

    SET HANDLER: obj_events->on_click FOR obj_alv.


    CALL METHOD obj_alv->set_table_for_first_display
      EXPORTING
        is_layout                     = wa_layout
        is_variant                    = wa_variant
        i_save                        = abap_true
      CHANGING
        it_outtab                     = it_0066[]
        it_fieldcatalog               = it_fcat[]
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ELSE.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.

********&---------------------------------------------------------------------*
********&      Module  CHECK_OBRIGATORIO  INPUT
********&---------------------------------------------------------------------*
********       text
********----------------------------------------------------------------------*
MODULE check_obrigatorio INPUT.

  o_form_lote->complementa_dados_header( CHANGING header = it_new ).
  o_form_lote->modifica_alv( EXPORTING acao = lv_acao
                                       header = it_new
                                       obj_alv = obj_alv
                             CHANGING t_0066 = it_0066[]
                                       ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  F4_INSTRUCAO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f4_instrucao INPUT.
  o_form_lote->get_0045( EXPORTING seq =  o_form_lote->busca_instrucoes( EXPORTING nro_solicitacao = s_solov-low
                                                                                   value_edit = l_value_edit
                                                                         IMPORTING referencia = l_referencia
                                                                         CHANGING header = it_new
                                                                                  t_0066 = it_0066[]
                                                                                  t_0328 = t_zsdt0328
                                                                                  t_alt_qtd = it_alt_qtd )
                                   t_0328 = t_zsdt0328[]
                                   referencia = l_referencia
                        CHANGING header = it_new
                                    ).
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_0045  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_0045 INPUT.
  o_form_lote->get_0045( EXPORTING instrucao = it_new-instrucao
                                   referencia = l_referencia
                         CHANGING header = it_new ).
  o_form_lote->complementa_dados_header( CHANGING header = it_new ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CHECK_ZMENG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_zmeng INPUT.

  PERFORM f_check_zmeng.  "*-CS2023000189-04.09.2023-#122555-JT-inicio

ENDMODULE.

*-CS2023000189-04.09.2023-#122555-JT-inicio
FORM f_check_zmeng.

*  FREE: IT_ALT_QTD.

  o_form_lote->complementa_dados_header( CHANGING header = it_new ).

  TRY .

      o_form_lote->check_zmeng( EXPORTING header = it_new
                                CHANGING t_0066 = it_0066[] ).
      CHECK lv_acao EQ 'BNT_QTD'.

      APPEND CORRESPONDING #( it_new ) TO it_alt_qtd.

    CATCH zcl_cx_excecoes_cad_form_lote INTO DATA(lo_excecao2).
      MESSAGE lo_excecao2 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
  ENDTRY.

ENDFORM.
*-CS2023000189-04.09.2023-#122555-JT-fim

FORM limpa_variaveis.

  FREE: it_save,
            it_0066,
            it_0051,
            it_new,
            it_edit,
            it_dele,
            it_0066_old,
            t_zsdt0328,
            it_log_old,
            it_alt_qtd,
            tg_save_log,
            wa_0066_old,
            wa_0066,
            wa_variant,
            wa_layout,
            wa_stable,
            it_fcat,
            tg_msg_ret,
            wg_mensagem,
            lv_acao,
            cont,
            str_l,
            campo,
            tam.

  UNASSIGN: <fs_campo>,
            <fs_campo1>.

ENDFORM.
