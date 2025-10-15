* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZLESR0158                                              *
* Title.......: Relatorios de Transportes - PNL                        *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 17/06/2022                                             *
* -------------------------------------------------------------------- *

REPORT zlesr0158.

TYPE-POOLS: slis, abap, icon.

TYPES: BEGIN OF ty_j_1bbranch,
         branch TYPE j_1bbranch-branch,
         name   TYPE j_1bbranch-name,
       END OF   ty_j_1bbranch,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF   ty_kna1,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF   ty_lfa1,

       BEGIN OF ty_makt,
         matnr TYPE makt-matnr,
         maktx TYPE makt-maktx,
       END OF   ty_makt.


TABLES: zlese0158, sscrfields.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZLESE0158'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.

"dados alv
DATA gt_dados_alv TYPE STANDARD TABLE OF zlese0158.
DATA gt_fieldcat TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2 TYPE TABLE OF bapiret2.


DATA: lit_branch TYPE TABLE OF ty_j_1bbranch,
      lit_kna1   TYPE TABLE OF ty_kna1,
      lit_lfa1   TYPE TABLE OF ty_lfa1,
      lit_makt   TYPE TABLE OF ty_makt.

"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS so_bukrs FOR zlese0158-empresa OBLIGATORY NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS so_branc FOR zlese0158-filial NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS so_dt_tr FOR zlese0158-data_transporte OBLIGATORY.
SELECT-OPTIONS so_id_tr FOR zlese0158-id_transporte.
SELECT-OPTIONS so_forne FOR zlese0158-fornecimento NO INTERVALS.
SELECT-OPTIONS so_tknum FOR zlese0158-tknum NO INTERVALS.
SELECT-OPTIONS so_fknum FOR zlese0158-fknum NO INTERVALS.
SELECT-OPTIONS so_matnr FOR zlese0158-produto NO-EXTENSION NO INTERVALS.
SELECT-OPTIONS so_nr_ov FOR zlese0158-nr_ov NO INTERVALS.
SELECT-OPTIONS so_nr_pe FOR zlese0158-nr_pedido NO INTERVALS.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

SELECT-OPTIONS so_dt_re FOR zlese0158-data_reg.
SELECT-OPTIONS so_hr_re FOR zlese0158-hora_reg.

SELECT-OPTIONS so_dt_at FOR zlese0158-dt_atualizacao.
SELECT-OPTIONS so_hr_at FOR zlese0158-hr_atualizacao.


SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
PARAMETERS p_cance RADIOBUTTON GROUP rg01.
PARAMETERS p_ativo RADIOBUTTON GROUP rg01.
PARAMETERS p_todos RADIOBUTTON GROUP rg01 DEFAULT 'X'.
PARAMETERS p_vari TYPE slis_vari NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b3.

INITIALIZATION.
  PERFORM f_preenche_data.
  "PERFORM F_BOTAO_FUNCTION.
  PERFORM default_variant CHANGING p_vari.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.
  PERFORM: f_seleciona,
           f_processa_saida.

END-OF-SELECTION.
  PERFORM f_exibe_alv.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_seleciona .

  DATA lr_canc TYPE RANGE OF zsde_cancelado.

  CASE 'X'.
    WHEN p_cance.
      APPEND 'IEQX' TO lr_canc.
    WHEN p_ativo.
      APPEND 'IEQ' TO lr_canc.
  ENDCASE.

  SELECT * FROM zlest0223
    INTO CORRESPONDING FIELDS OF TABLE gt_dados_alv
      WHERE empresa         IN so_bukrs
        AND filial          IN so_branc
        AND data_transporte IN so_dt_tr
        AND id_transporte   IN so_id_tr
        AND fornecimento    IN so_forne
        AND tknum           IN so_tknum
        AND fknum           IN so_fknum
        AND produto         IN so_matnr
        AND nr_ov           IN so_nr_ov
        AND nr_pedido       IN so_nr_pe
        AND data_reg        IN so_dt_re
        AND hora_reg        IN so_hr_re
        AND dt_atualizacao  IN so_dt_at
        AND hr_atualizacao  IN so_hr_at
        AND cancelado       IN lr_canc.

  CHECK gt_dados_alv[] IS NOT INITIAL.

*-------------------------------------------------------------------------*
*  Local Embarque
*-------------------------------------------------------------------------*
  DATA(lit_dados_alv_aux) = gt_dados_alv[].
  DELETE lit_dados_alv_aux WHERE local_embarque IS INITIAL.

  IF lit_dados_alv_aux[] IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE lit_lfa1
        FOR ALL ENTRIES IN lit_dados_alv_aux
     WHERE lifnr EQ lit_dados_alv_aux-local_embarque.
  ENDIF.

*-------------------------------------------------------------------------*
*  Local Entrega
*-------------------------------------------------------------------------*
  lit_dados_alv_aux = gt_dados_alv[].
  DELETE lit_dados_alv_aux WHERE local_entrega IS INITIAL.

  IF lit_dados_alv_aux[] IS NOT INITIAL.
    SELECT kunnr name1
      FROM kna1 APPENDING CORRESPONDING FIELDS OF TABLE lit_kna1
        FOR ALL ENTRIES IN lit_dados_alv_aux
     WHERE kunnr EQ lit_dados_alv_aux-local_entrega.
  ENDIF.

*-------------------------------------------------------------------------*
*  Porto Destino
*-------------------------------------------------------------------------*
  lit_dados_alv_aux = gt_dados_alv[].
  DELETE lit_dados_alv_aux WHERE porto_destino IS INITIAL.

  IF lit_dados_alv_aux[] IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE lit_lfa1
       FOR ALL ENTRIES IN lit_dados_alv_aux
     WHERE lifnr EQ lit_dados_alv_aux-porto_destino.
  ENDIF.

*-------------------------------------------------------------------------*
* Produto
*-------------------------------------------------------------------------*
  lit_dados_alv_aux = gt_dados_alv[].
  DELETE lit_dados_alv_aux WHERE produto IS INITIAL.

  IF lit_dados_alv_aux[] IS NOT INITIAL.
    SELECT matnr maktx
      FROM makt APPENDING CORRESPONDING FIELDS OF TABLE lit_makt
       FOR ALL ENTRIES IN lit_dados_alv_aux
     WHERE matnr EQ lit_dados_alv_aux-produto
       AND spras EQ sy-langu.
  ENDIF.

*-------------------------------------------------------------------------*
* Filial
*-------------------------------------------------------------------------*
  lit_dados_alv_aux = gt_dados_alv[].
  DELETE lit_dados_alv_aux WHERE filial IS INITIAL.

  IF lit_dados_alv_aux[] IS NOT INITIAL.
    SELECT branch name
      FROM j_1bbranch APPENDING CORRESPONDING FIELDS OF TABLE lit_branch
       FOR ALL ENTRIES IN lit_dados_alv_aux
     WHERE branch EQ lit_dados_alv_aux-filial.
  ENDIF.

*-------------------------------------------------------------------------*
* Agente Frete
*-------------------------------------------------------------------------*
  lit_dados_alv_aux = gt_dados_alv[].
  DELETE lit_dados_alv_aux WHERE tdlnr IS INITIAL.

  IF lit_dados_alv_aux[] IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE lit_lfa1
       FOR ALL ENTRIES IN lit_dados_alv_aux
     WHERE lifnr EQ lit_dados_alv_aux-tdlnr.
  ENDIF.

*-------------------------------------------------------------------------*
* Proprietario Veiculo
*-------------------------------------------------------------------------*
  lit_dados_alv_aux = gt_dados_alv[].
  DELETE lit_dados_alv_aux WHERE propr_veiculo IS INITIAL.

  IF lit_dados_alv_aux[] IS NOT INITIAL.
    SELECT lifnr name1
      FROM lfa1 APPENDING CORRESPONDING FIELDS OF TABLE lit_lfa1
       FOR ALL ENTRIES IN lit_dados_alv_aux
     WHERE lifnr EQ lit_dados_alv_aux-propr_veiculo.
  ENDIF.


ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN '&IC1'.
      PERFORM f_hyperlink   USING rs_selfield.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

  rs_selfield-refresh = 'X'.
  r_ucomm = '&REFRESH'.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_FUNCTION
*&---------------------------------------------------------------------*
FORM f_botao_function.

  sscrfields-functxt_01 = 'BOTAO 1'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_COMMAND
*&---------------------------------------------------------------------*
FORM f_botao_command.

  IF sy-ucomm = 'FC01'.
    "EXECUTA FUNÇÃO DO BOTAO 1
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA lw_layout TYPE slis_layout_alv.
  DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

    IF p_vari IS NOT INITIAL.
      lw_variant-report = sy-repid.
      lw_variant-variant = p_vari.
    ENDIF.

    PERFORM f_monta_fieldcat.

    lw_layout-zebra             = abap_true.
    lw_layout-colwidth_optimize = abap_true.
    lw_layout-box_fieldname = gc_select_field.
    lw_layout-info_fieldname = 'COLOR'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'F_STATUS_SET'
        i_callback_user_command  = 'F_USER_COMMAND'
        is_layout                = lw_layout
        it_fieldcat              = gt_fieldcat
        i_save                   = 'A'
        is_variant               = lw_variant
      TABLES
        t_outtab                 = gt_dados_alv
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema.
    ENDIF.

  ELSE.
    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.

  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
*&---------------------------------------------------------------------*
*&      Form  F_PFSTATUS
*&---------------------------------------------------------------------*
FORM f_status_set USING p_extab TYPE slis_t_extab.          "#EC CALLED

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat.


  "LVC_FIELDCATALOG_MERGE
  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = gc_internal_tab
      i_structure_name       = gc_struc_name
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = gc_select_field.
  DELETE gt_fieldcat WHERE fieldname = 'MSGTX'.
  DELETE gt_fieldcat WHERE fieldname = 'ICON'.


  PERFORM f_coluna_edita    USING 1  'ID_TRANSPORTE'  'Id. Transporte'.
  PERFORM f_coluna_edita    USING 2  'TRECHO'  'Trecho'.
  PERFORM f_coluna_edita    USING 3  'FORNECIMENTO'  'Fornecimento'.
  PERFORM f_coluna_edita    USING 4  'FKNUM'  'Doc. Custo'.
  PERFORM f_coluna_edita    USING 5  'TP_TRECHO'  'Tp.Trecho'.

  PERFORM f_coluna_edita    USING 6  'PORTO_DESTINO'  'Posto Destino'.
  PERFORM f_coluna_edita    USING 7  'DS_PORTO_DESTINO'  'Ds.Posto Destino'.

  PERFORM f_coluna_edita    USING 8  'LOCAL_EMBARQUE'     'Loc.Embarque'.
  PERFORM f_coluna_edita    USING 9  'DS_LOCAL_EMBARQUE'  'Ds.Loc.Embarque'.

  PERFORM f_coluna_edita    USING 10  'LOCAL_ENTREGA'  'Loc.Entrega'.
  PERFORM f_coluna_edita    USING 11  'DS_LOCAL_ENTREGA'  'Ds.Loc.Entrega'.


  PERFORM f_coluna_edita    USING 12  'PRODUTO'     'Produto'.
  PERFORM f_coluna_edita    USING 13  'DS_PRODUTO'  'Ds.Produto'.

  PERFORM f_coluna_edita    USING 14  'EMPRESA'  'Empresa'.

  PERFORM f_coluna_edita    USING 15  'FILIAL'  'Filial'.
  PERFORM f_coluna_edita    USING 16  'DS_FILIAL'	'Ds.Filial'.



  PERFORM f_coluna_edita    USING 17  'SAFRA'  'Safra'.
  PERFORM f_coluna_edita    USING 18  'VBTYP_V'  'Tp.Fornec.'.
  PERFORM f_coluna_edita    USING 19  'TKNUM'  'Doc.Transp.'.
  PERFORM f_coluna_edita    USING 20  'DATA_TRANSPORTE'  'Data Transp.'.
  PERFORM f_coluna_edita    USING 21  'DATA_CUSTO'  'Data Custo'.
  PERFORM f_coluna_edita    USING 22  'VSART'  'Tipo Exp.'.

  PERFORM f_coluna_edita    USING 23  'TDLNR'            'Agente Frete'.
  PERFORM f_coluna_edita    USING 24  'DS_AGENTE_FRETE'  'Ds.Agente Frete'.


  PERFORM f_coluna_edita    USING 25  'SHTYP'  'Tp.Transp.'.

  PERFORM f_coluna_edita    USING 26  'PROPR_VEICULO'     'Prop. Veic.'.
  PERFORM f_coluna_edita    USING 27  'DS_PROP_VEICULO'   'Ds.Prop. Veic.'.


  PERFORM f_coluna_edita    USING 28  'QTDE_TOTAL'  'Qtd.Total'.
  PERFORM f_coluna_edita    USING 29  'QTDE_UTILIZADA'  'Qtd.Utilizada'.
  PERFORM f_coluna_edita    USING 30  'SALDO_TRANSB'  'Saldo Transb.'.
  PERFORM f_coluna_edita    USING 31  'UNIDADE'  'Unidade'.
  PERFORM f_coluna_edita    USING 32  'VL_BRL'  'Vlr. BRL'.
  PERFORM f_coluna_edita    USING 33  'VL_USD'  'Vlr. USD'.
  PERFORM f_coluna_edita    USING 34  'TRANSBORDO'  'Transbordo'.
  PERFORM f_coluna_edita    USING 35  'TRANSB_FORA_FILIAL'  'Transb.Fora Filial'.
  PERFORM f_coluna_edita    USING 36  'SLD_TRANSB_REVERTIDO'  'Sld.Transb.Revertido'.
  PERFORM f_coluna_edita    USING 37  'REALIZADO'  'Realizado'.
  PERFORM f_coluna_edita    USING 38  'AWAIT_INFO'  'Aguard.Info.'.
  PERFORM f_coluna_edita    USING 39  'CH_REFERENCIA_ROM'  'Chave. Ref. Rom.'.
  PERFORM f_coluna_edita    USING 40  'NR_OV'  'Nr. OV'.
  PERFORM f_coluna_edita    USING 41  'NR_PEDIDO'  'Nr. Pedido'.
  PERFORM f_coluna_edita    USING 42  'CHAVE_TRECHO_REF'  'Chave Trecho Ref.'.

  PERFORM f_coluna_edita    USING 43  'USER_REG'  'Usuario Criação'.
  PERFORM f_coluna_edita    USING 44  'DATA_REG'  'Data Criação'.
  PERFORM f_coluna_edita    USING 45  'HORA_REG'  'Hora Criação'.

  PERFORM f_coluna_edita    USING 46  'DT_ATUALIZACAO'  'Data Atualização'.
  PERFORM f_coluna_edita    USING 47  'HR_ATUALIZACAO'  'Hora Atualização'.
  PERFORM f_coluna_edita    USING 48  'US_ATUALIZACAO'  'Usuário Atualização'.

  PERFORM f_coluna_edita    USING 49  'CANCELADO'  'Cancelado'.
  PERFORM f_coluna_edita    USING 50  'DT_CANCELAMENTO' 'Data Cancelamento'.
  PERFORM f_coluna_edita    USING 51  'HR_CANCELAMENTO' 'Hora Cancelamento'.

  PERFORM f_coluna_edita    USING 52  'SINCRONIZADO_SIGAM'  'Sinc. SIGAM'.
  PERFORM f_coluna_edita    USING 53  'DT_SINCRONIA_SIGAM'  'Dt.Sinc.SIGAM'.
  PERFORM f_coluna_edita    USING 54  'HR_SINCRONIA_SIGAM'  'Hr.Sinc.SIGAM'.


ENDFORM.                    " F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema.

  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_s.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_SISTEMA_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_insere.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING sy-msgty
           sy-msgid
           sy-msgno
           sy-msgv1
           sy-msgv2
           sy-msgv3
           sy-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c
                           p_percent TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percent
      text       = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
FORM f4_for_variant CHANGING f_vari TYPE slis_vari.

  DATA: lw_variant TYPE disvariant.

  lw_variant-variant = f_vari.
  lw_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = lw_variant
      i_save     = 'A'
    IMPORTING
      es_variant = lw_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    "f4_for_variant
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VARIANT
*&---------------------------------------------------------------------*
FORM default_variant CHANGING f_vari TYPE slis_vari.
  DATA: lw_variant TYPE disvariant.

  lw_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = lw_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc = 0.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    " DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
FORM f_preenche_data .

  DATA(lv_ini) = sy-datum.
  DATA(lv_fim) = sy-datum.

  CHECK so_dt_tr[] IS INITIAL.

  SUBTRACT 30 FROM lv_ini.

  APPEND 'IBT' && lv_ini && lv_fim TO so_dt_tr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CASE rs_selfield-fieldname.
    WHEN 'COLUNA'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_HYPERLINK

"PERFORM f_mensagem_insere TABLES p_ret2
"USING 'E' 'ZMM' '000' 'SYSID' text-t02
"gw_034-logsys space space.

FORM f_mensagem_bapiret USING p_mess TYPE bapiret2.

  MESSAGE ID p_mess-id TYPE 'S' NUMBER p_mess-number
    WITH p_mess-message_v1 p_mess-message_v2
         p_mess-message_v3 p_mess-message_v4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere TABLES p_ret_tab STRUCTURE bapiret2
                        USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO p_ret_tab ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
    WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
      <fs_ret>-message_v4 INTO <fs_ret>-message.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe_popup USING p_bapiret2_tab TYPE bapiret2_t.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  IF l_lines <= 1 OR sy-batch = 'X'.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE 'S'
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.     "#EC CI_SUBRC

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        OTHERS    = 5.     "#EC CI_SUBRC

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.     "#EC CI_SUBRC

  ENDIF.

ENDFORM.
* CONTROLE DE ATUALIZAÇÃO DE TELA DINAMICAMENTE

*    DATA lt_return TYPE TABLE OF ddshretval.
*    DATA lt_fields TYPE TABLE OF dynpread.
*
*    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*      EXPORTING
*        tabname           = 'ZTPP_009'
*        fieldname         = 'MATNR_DUMMY'
*        "searchhelp        = 'ZHPP_DUMMY'
*        "shlpparam         = 'MATNR_DUMMY'
*        "IMPORTING
*        "user_reset        =
*      TABLES
*        return_tab        = lt_return
*      EXCEPTIONS
*        field_not_found   = 1
*        no_help_for_field = 2
*        inconsistent_help = 3
*        no_values_found   = 4
*        OTHERS            = 5.
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*
*      APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn>).
*
**      stepl
**
**      fieldinp
*
*      CASE <fs_ret>-fieldname.
*        WHEN 'WERKS'.
*          <fs_dyn>-fieldname = 'P_WERKS'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'ARBPL'.
*          <fs_dyn>-fieldname = 'P_ARBPL'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'VORNR'.
*
*          <fs_dyn>-fieldname = 'P_VORNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*        WHEN 'FLAG_DUMPS'.
*
*          APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn2>).
*
*          IF <fs_ret>-fieldval = 'X'.
*
*            <fs_dyn>-fieldname = 'P_DUM_S'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_N'.
*            <fs_dyn2>-fieldvalue = space.
*
*          ELSE.
*            <fs_dyn>-fieldname = 'P_DUM_N'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_S'.
*            <fs_dyn2>-fieldvalue = space.
*          ENDIF.
*
*        WHEN 'MATNR_DUMMY'.
*
*          <fs_dyn>-fieldname = 'P_MATNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*
*      ENDCASE.
*
*    ENDLOOP.
*
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        dyname               = '1000'
*        dynumb               = '1000'
*      TABLES
*        dynpfields           = lt_fields
*      EXCEPTIONS
*        invalid_abapworkarea = 1
*        invalid_dynprofield  = 2
*        invalid_dynproname   = 3
*        invalid_dynpronummer = 4
*        invalid_request      = 5
*        no_fielddescription  = 6
*        undefind_error       = 7
*        OTHERS               = 8.
*
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


***AT SELECTION-SCREEN OUTPUT.
**
**    LOOP AT SCREEN.
**
**      IF screen-name CP '*P_WERKS*'
**        OR screen-name CP '*P_DUM_N*'
**        OR screen-name CP '*P_ARBPL*'
**        OR screen-name CP '*P_VORNR*'.
**
**        screen-input = 0.
**        MODIFY SCREEN.
**      ENDIF.
**
**    ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita  USING p_col_pos
                           p_fieldname TYPE slis_fieldname
                           p_text TYPE scrtext_l.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-col_pos   = p_col_pos.
  <fs_cat>-seltext_s = p_text.
  <fs_cat>-seltext_m = p_text.
  <fs_cat>-seltext_l = p_text.
  <fs_cat>-reptext_ddic = p_text.

ENDFORM.

FORM f_append_fieldcat USING VALUE(p_col_pos)       TYPE i
                             VALUE(p_ref_tabname)   LIKE dd02d-tabname
                             VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                             VALUE(p_tabname)       LIKE dd02d-tabname
                             VALUE(p_field)         LIKE dd03d-fieldname
                             VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                             VALUE(p_outputlen)
                             VALUE(p_hotspot).

  APPEND INITIAL LINE TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fiedcat>).

  <fs_fiedcat>-fieldname     = p_field.
  <fs_fiedcat>-tabname       = p_tabname.
  <fs_fiedcat>-ref_tabname   = p_ref_tabname.
  <fs_fiedcat>-ref_fieldname = p_ref_fieldname.
  <fs_fiedcat>-key           = ' '.
  <fs_fiedcat>-key_sel       = 'X'.
  <fs_fiedcat>-col_pos       = p_col_pos.
  <fs_fiedcat>-no_out        = ' '.
  <fs_fiedcat>-seltext_s     = p_scrtext_l.
  <fs_fiedcat>-seltext_m     = p_scrtext_l.
  <fs_fiedcat>-seltext_l     = p_scrtext_l.
  <fs_fiedcat>-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    <fs_fiedcat>-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  <fs_fiedcat>-fieldname     TO UPPER CASE.
  TRANSLATE  <fs_fiedcat>-tabname       TO UPPER CASE.
  TRANSLATE  <fs_fiedcat>-ref_tabname   TO UPPER CASE.
  TRANSLATE  <fs_fiedcat>-ref_fieldname TO UPPER CASE.



ENDFORM.                    " MONTAR_ESTRUTURA



*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

  READ TABLE gt_dados_alv WITH KEY selec = 'X' TRANSPORTING NO FIELDS.

  IF sy-subrc NE 0.
    MESSAGE s851(v4) DISPLAY LIKE 'E'.
    p_error = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa
*&---------------------------------------------------------------------*
FORM f_processa.

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING text-t01 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.



  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt USING i_type TYPE bapi_mtype
                                 p_string TYPE string.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere
    TABLES gt_bapiret2
     USING i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

ENDFORM.

FORM f_processa_saida .

  SORT: lit_branch by branch,
        lit_kna1   by kunnr,
        lit_lfa1   by lifnr,
        lit_makt   by matnr.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados_alv>).

    READ TABLE lit_lfa1 INTO DATA(lwa_lfa1) WITH KEY lifnr = <fs_dados_alv>-local_embarque BINARY SEARCH.
    IF sy-subrc eq 0.
      <fs_dados_alv>-ds_local_embarque = lwa_lfa1-name1.
    ENDIF.

    READ TABLE lit_kna1 INTO DATA(lwa_kna1) WITH KEY kunnr = <fs_dados_alv>-local_entrega BINARY SEARCH.
    IF sy-subrc eq 0.
      <fs_dados_alv>-ds_local_entrega = lwa_kna1-name1.
    ENDIF.

    READ TABLE lit_lfa1 INTO lwa_lfa1 WITH KEY lifnr = <fs_dados_alv>-porto_destino BINARY SEARCH.
    IF sy-subrc eq 0.
      <fs_dados_alv>-ds_porto_destino = lwa_lfa1-name1.
    ENDIF.

    READ TABLE lit_makt INTO DATA(lwa_makt) WITH KEY matnr = <fs_dados_alv>-produto BINARY SEARCH.
    IF sy-subrc eq 0.
      <fs_dados_alv>-ds_produto = lwa_makt-maktx.
    ENDIF.

    READ TABLE lit_branch INTO DATA(lwa_branch) WITH KEY branch = <fs_dados_alv>-filial BINARY SEARCH.
    IF sy-subrc eq 0.
      <fs_dados_alv>-ds_filial = lwa_branch-name.
    ENDIF.

    READ TABLE lit_lfa1 INTO lwa_lfa1 WITH KEY lifnr = <fs_dados_alv>-tdlnr BINARY SEARCH.
    IF sy-subrc eq 0.
      <fs_dados_alv>-ds_agente_frete = lwa_lfa1-name1.
    ENDIF.

    READ TABLE lit_lfa1 INTO lwa_lfa1 WITH KEY lifnr = <fs_dados_alv>-propr_veiculo BINARY SEARCH.
    IF sy-subrc eq 0.
      <fs_dados_alv>-ds_prop_veiculo = lwa_lfa1-name1.
    ENDIF.

  ENDLOOP.

ENDFORM.
