class ZCL_CADASTRO_FORMACAO_LOTE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_0066.
    TYPES: icon(4)       TYPE c.
    TYPES: status_trace(4) TYPE c.
           INCLUDE TYPE zsdt0066.
    TYPES: color(4)        TYPE c,
           werks_desc      TYPE name1,
           matnr_desc      TYPE maktx,
           terminal_desc   TYPE name1,
           ponto_c_desc    TYPE name1,
           lentrega_desc   TYPE name1,
           kunnr_desc      TYPE name1,
           btgew           TYPE gsgew.

    TYPES END OF ty_0066 .
  types:
    BEGIN OF ty_lotes,
        seq                  TYPE zsdt0045-zseq_inst,
        objek                TYPE objnum,
        objecttable          TYPE tabelle,
        referencia           TYPE numc10,
        instrucao            TYPE zsded030,
        matnr                TYPE matnr,
        werks                TYPE werks_ext,
        quantidade           TYPE numc10,
        quantidade_disp      TYPE int4,
        quantidade_util      TYPE int4,
        quantidade_disp_orig TYPE int4,
        quantidade_util_orig TYPE int4,
        dmbtr                TYPE char20,
        btgew                TYPE gsgew,
        gewei                TYPE gewei,
        inco1                TYPE zsdt0045-incoterm,
        region               TYPE t001w-regio,
        charg                TYPE charg_d,
        contrato             TYPE text50,
      END OF ty_lotes .
  types:
    BEGIN OF ty_0051.
        INCLUDE TYPE zsdt0051.
    TYPES: _nro_sol_ov TYPE zsdt0045-objek,
        _bstkd      TYPE zsdt0045-contrato.
    TYPES END OF  ty_0051 .
  types:
    tb_0051 TYPE TABLE OF ty_0051 .
  types:
    tb_0066 TYPE TABLE OF ty_0066 .
  types:
    tb_0327 TYPE TABLE OF zsdt0327 .
  types:
    tb_0328 TYPE TABLE OF zsdt0328 .
  types:
    tb_new  TYPE TABLE OF ty_0066 .
  types:
    tb_save      TYPE TABLE OF zsdt0066 .
  types:
    tb_zfiwrs0002 TYPE TABLE OF zfiwrs0002 .
  types:
    tb_lotes TYPE TABLE OF ty_lotes .

  data AT_LIBRA_TO type ZSDED040 .
  data AT_LIB_TO_F type ZSDED040 .
  data T_0066_OLD type TB_0066 .

  methods CONSTRUCTOR .
  methods BLOQUEIA_NRO_SOLICITACAO
    importing
      !NRO_SOLICITACAO type ZSDED013
    exporting
      !SUCESSO type C .
  methods DESBLOQUEIA_NRO_SOLICITACAO
    importing
      !NRO_SOLICITACAO type ZSDED013
    exporting
      !SUCESSO type C .
  methods BUSCA_INSTRUCOES
    importing
      !NRO_SOLICITACAO type ZSDED013 optional
      !ACAO type SY-UCOMM optional
      !VALUE_EDIT type SY-TABIX optional
      !BACKGROUND type C optional
    exporting
      !REFERENCIA type TY_0066-REFERENCIA
    changing
      !T_0328 type TB_0328 optional
      !T_0066 type TB_0066 optional
      !HEADER type TY_0066 optional
      !T_ALT_QTD type TB_SAVE optional
      !T_INSTRUCOES type TB_LOTES optional
    returning
      value(SEQ) type ZSEQ_INST
    raising
      ZCL_CX_EXCECOES_CAD_FORM_LOTE .
  methods SELECIONA_DADOS_GERAIS
    importing
      !NRO_SOLICITACAO type ZSDED013
    exporting
      !T_0066 type TB_0066
      !T_0327 type TB_0327
      !T_0328 type TB_0328
      !HEADER type TY_0066
    raising
      ZCL_CX_EXCECOES_CAD_FORM_LOTE .
  methods BUSCA_DADOS_HEADER
    importing
      !NRO_SOLICITACAO type ZSDED013
    returning
      value(W_0051) type TY_0051 .
  methods BUSCA_DESCRICOES
    importing
      !CAMPO type STRING
    changing
      !VALOR type ANY
    returning
      value(DESCRICAO) type STRING .
  methods COMPLEMENTA_DADOS_HEADER
    changing
      !HEADER type TY_0066 .
  methods CALCULA_LOTE
    importing
      !HEADER type TY_0066
    returning
      value(VLRTOT) type DMBTR .
  methods CALCULA_VLR_USD
    changing
      !HEADER type TY_0066 .
  methods GRAVA_LOG
    importing
      !T_0066 type TB_SAVE
      !NRO_SOLICITACAO type ZSDED013 .
  methods INPUT_LOG
    importing
      !NRO_SOLICITACAO type ZSDED013
      !VALUE1 type LVC_FNAME optional
      !VALUE2 type CHAR20 optional
      !VALUE3 type SY-TABIX optional .
  methods GET_SEQ
    importing
      !ID type NROBJ
    returning
      value(R_VALUE) type ZSEQ_INST .
  methods GET_DADOS_TRACE
    changing
      !T_0066 type TB_0066 .
  methods GET_POSNR
    importing
      !NRO_SOLICITACAO type ZSDED013 optional
      !T_SAVE type TB_SAVE optional
      !REFERENCIA type NUMC10 optional
    changing
      !T_0328 type TB_0328 optional
    returning
      value(POSNR) type POSNR_VA .
  methods MONTA_MATCHCODE
    importing
      !CAMPO type DFIES-FIELDNAME
    returning
      value(VALOR) type SHVALUE_D .
  methods CHECK_ZMENG
    importing
      !HEADER type TY_0066
    changing
      !T_0066 type TB_0066
    raising
      ZCL_CX_EXCECOES_CAD_FORM_LOTE .
  methods GET_0045
    importing
      !SEQ type ANY optional
      !INSTRUCAO type ANY optional
      !REFERENCIA type NUMC10 optional
      !T_0328 type TB_0328 optional
    changing
      !HEADER type TY_0066 .
  methods GET_LOCAL_ENTREGA
    importing
      !LIFNR type LIFNR
    returning
      value(LOCAL_ENTREGA) type KUNNR .
  methods SET_LAYOUT
    changing
      !LAYOUT type LVC_S_LAYO
      !VARIANT type DISVARIANT
      !STABLE type LVC_S_STBL .
  methods EXIBE_MENSSAGENS
    importing
      !T_FCAT type LVC_T_FCAT
      !ACAO type SY-UCOMM
      !T_0066 type TB_0066
    exporting
      !TEXTO_BT_DINAMICO type CHAR30
    changing
      !T_MSG_RET type TB_ZFIWRS0002
      !HEADER type TY_0066 .
  methods SET_ERROS
    importing
      !T_FCAT type LVC_T_FCAT
      !ACAO type SY-UCOMM
      !T_0066 type TB_0066
    exporting
      !TEXTO_BT_DINAMICO type CHAR30
    changing
      !T_MSG_RET type TB_ZFIWRS0002
      !HEADER type TY_0066 .
  methods ADD_NOVO_LOTE
    importing
      !ACAO type SY-UCOMM optional
      !BACKGROUND type CHAR1 optional
    exporting
      !TEXTO_BT_DINAMICO type CHAR30
    changing
      !HEADER type TY_0066 optional
      !T_0066 type TB_0066 optional
      !T_MSG_RET type TB_ZFIWRS0002 optional
      !T_FCAT type LVC_T_FCAT optional .
  methods SAVE
    importing
      !ACAO type SY-UCOMM optional
      !T_FCAT type LVC_T_FCAT optional
      !NRO_SOLICITACAO type ZSDED013 optional
      !BACKGROUND type CHAR1 optional
    changing
      !T_0066 type TB_0066 optional
      !T_0328 type TB_0328 optional
      !T_MSG_RET type TB_ZFIWRS0002 optional
      !HEADER type TY_0066 optional
      !T_0327 type TB_0327 optional
      !T_ALT_QTD type TB_SAVE optional .
  methods ATUALIZA_QTD
    importing
      !T_0066 type TB_SAVE
    returning
      value(ERROR) type SY-SUBRC .
  methods EDIT_LOTE
    importing
      !OBJ_ALV type ref to CL_GUI_ALV_GRID
    changing
      !T_0066 type TB_0066
      !ACAO type SY-UCOMM
      !HEADER type TY_0066
    raising
      ZCL_CX_EXCECOES_CAD_FORM_LOTE .
  methods GET_INDEX
    importing
      !OBJ_ALV type ref to CL_GUI_ALV_GRID
    changing
      !ACAO type SY-UCOMM
    returning
      value(INDEX) type SY-TABIX
    raising
      ZCL_CX_EXCECOES_CAD_FORM_LOTE .
  methods DELETA_LOTE
    importing
      !OBJ_ALV type ref to CL_GUI_ALV_GRID
    changing
      !T_0066 type TB_0066
      !ACAO type SY-UCOMM
      !T_DELE type TB_SAVE .
  methods REENVIA_TRACE
    importing
      !OBJ_ALV type ref to CL_GUI_ALV_GRID
    changing
      !T_0066 type TB_0066 .
  methods MODIFICA_ALV
    importing
      !OBJ_ALV type ref to CL_GUI_ALV_GRID
      !ACAO type SY-UCOMM
      !HEADER type TY_0066
    changing
      !T_0066 type TB_0066 .
  methods LIBERAR_LOTE
    importing
      !OBJ_ALV type ref to CL_GUI_ALV_GRID optional
      !NRO_SOLICITACAO type ZSDED013 optional
      !BACKGROUND type CHAR01 optional
    exporting
      !MENSAGEM type CHAR100
    changing
      !ACAO type SY-UCOMM optional
      !T_0066 type TB_0066 optional .
  methods CHECK_BOTAO
    changing
      !ACAO type SY-UCOMM optional
      !T_0066 type TB_0066 .
  methods ALTERA_QTD
    importing
      !T_0066 type TB_0066
    changing
      !HEADER type TY_0066
      !ACAO type SY-UCOMM .
  methods BLOQ_DESBLOQ_CAMPOS_TELA
    importing
      !ACAO type SY-UCOMM .
protected section.
private section.

  data AT_HIST type ZSDED032 .
ENDCLASS.



CLASS ZCL_CADASTRO_FORMACAO_LOTE IMPLEMENTATION.


  METHOD add_novo_lote.

    IF background IS INITIAL.

      me->set_erros( EXPORTING t_fcat = t_fcat
                               acao   = acao
                               t_0066 = t_0066
                     IMPORTING texto_bt_dinamico = texto_bt_dinamico
                     CHANGING  header = header
                               t_msg_ret = t_msg_ret ).

      CHECK t_msg_ret[] IS INITIAL.

    ENDIF.

    APPEND VALUE #(
                    mandt               = sy-mandt
                    nro_sol_ov          = header-nro_sol_ov
                    instrucao           = header-instrucao
                    matnr               = header-matnr
                    matnr_desc          = header-matnr_desc
                    werks               = header-werks
                    werks_desc          = header-werks_desc
                    lgort               = header-lgort
                    ponto_c             = header-ponto_c
                    ponto_c_desc        = header-ponto_c_desc
                    charg               = header-charg
                    zmeng               = header-zmeng
                    zieme               = header-zieme
                    volum               = header-volum
                    voleh               = header-voleh
                    dmbtr               = header-dmbtr
                    pmein               = header-pmein
                    vlrtot              = header-vlrtot
                    waerk               = header-waerk
                    terminal            = header-terminal
                    terminal_desc       = header-terminal_desc
                    inco1               = header-inco1
                    inco2               = header-inco2
                    lentrega            = header-lentrega
                    lentrega_desc       = header-lentrega_desc
                    kunnr               = header-kunnr
                    kunnr_desc          = header-kunnr_desc
                    status              = 'A'
                    status_form         = header-status_form
                    libra_to            = header-libra_to
                    usd_to              = header-usd_to
                    vlr_tot_frm_usd     = header-vlr_tot_frm_usd
                    classificacao       = header-classificacao
                    auart               = header-auart
                    dco                 = header-dco
                    aviso               = header-aviso
                    usnam               = sy-uname
                    data_atual          = sy-datum
                    hora_atual          = sy-uzeit
                    color               = 'C511'
                    charg_ori           = header-charg_ori
                    referencia          = header-referencia    "*-CS2023000189-19.04.2023-#108709-JT
                  ) TO t_0066.

    CLEAR header.

  ENDMETHOD.


  METHOD altera_qtd.

*    CHECK me->get_index( EXPORTING obj_alv = obj_alv
*                         CHANGING acao = sy-ucomm ) IS NOT INITIAL.
*
*    IF it_0066[ me->get_index( EXPORTING obj_alv = obj_alv
*                               CHANGING acao = acao ) ]-vbeln IS INITIAL.
*      mensagem = |Formação de Lote não possui Ordem Gerada!|.
*      CLEAR acao.
*      EXIT.
*    ENDIF.
*
*    header = t_0066[ obj_fmlot->get_index( sy-ucomm ) ].
*
**    me->at_qtd = header-zmeng.

  ENDMETHOD.


  METHOD atualiza_qtd.

    CALL FUNCTION 'ZSDMF002_ATUALI_OV_SOLICITACAO'
      IMPORTING
        erro              = error
      TABLES
        ti_form_lote      = t_0066
      EXCEPTIONS
        ov_nao_encontrada = 1
        OTHERS            = 2.

  ENDMETHOD.


  METHOD bloqueia_nro_solicitacao.

    CALL FUNCTION 'ENQUEUE_EZSDT0066'
      EXPORTING
        nro_sol_ov     = nro_solicitacao
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc IS INITIAL.
      sucesso = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD bloq_desbloq_campos_tela.

    CHECK acao EQ 'BNT_QTD'.

    LOOP AT SCREEN.
      IF screen-name CS 'IT_NEW-'.
        CASE screen-name.
          WHEN 'IT_NEW-ZMENG'.
          WHEN 'IT_NEW-VOLUM'.
          WHEN OTHERS.
            screen-input = 0.
        ENDCASE.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD busca_dados_header.

    SELECT SINGLE * FROM zsdt0051 INTO w_0051 WHERE nro_sol_ov EQ nro_solicitacao.

  ENDMETHOD.


  METHOD busca_descricoes.
    DATA: lv_matnr18 TYPE matnr18.

    ASSIGN valor TO FIELD-SYMBOL(<fs_valor>).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <fs_valor>
      IMPORTING
        output = <fs_valor>.

    CASE campo.
      WHEN 'WERKS'.
        SELECT SINGLE name1 FROM t001w INTO descricao WHERE werks EQ <fs_valor>.
      WHEN 'MATNR'.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = valor
          IMPORTING
            output = lv_matnr18.

        SELECT SINGLE maktx FROM makt INTO descricao WHERE matnr EQ lv_matnr18 AND spras EQ sy-langu.
        IF sy-subrc IS NOT INITIAL.
          SELECT SINGLE maktx FROM makt INTO descricao WHERE matnr EQ <fs_valor> AND spras EQ sy-langu.
        ENDIF.
      WHEN 'TERMINAL' OR 'PONTO_C'.
        SELECT SINGLE name1 FROM lfa1 INTO descricao WHERE lifnr EQ <fs_valor>.
      WHEN 'LENTREGA' OR 'KUNNR'.
        SELECT SINGLE name1 FROM kna1 INTO descricao WHERE kunnr EQ <fs_valor>.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD busca_instrucoes.

    DATA: l_posnr       TYPE posnr_va,
          l_editar      TYPE char1,
          lw_0051       TYPE ty_0051,
          lv_nro_sol_ov TYPE zsdt0045-objek,
          lv_bstkd      TYPE zsdt0045-contrato,
          l_back        TYPE char1,
          lv_referencia TYPE ty_0066-referencia.

    IF background IS INITIAL.

      CLEAR referencia.

      MOVE-CORRESPONDING me->busca_dados_header( nro_solicitacao = nro_solicitacao
                                               ) TO lw_0051.

      lv_nro_sol_ov = lw_0051-nro_sol_ov.
      lv_bstkd      = lw_0051-bstkd.

      TYPES: BEGIN OF ty_f4,
               zseq_inst TYPE zseq_inst,
               instrucao TYPE zsded030,
               matnr     TYPE matnr,
               werks     TYPE werks_ext,
               ponto_c   TYPE lifnr,
               terminal  TYPE lifnr,
               charg     TYPE charg_d,
             END OF ty_f4.

      DATA: it_f4_ins TYPE TABLE OF ty_f4.

      IF header-posnr IS INITIAL AND header-referencia IS INITIAL.
        referencia = referencia + 1.
      ELSEIF header-posnr IS NOT INITIAL.
        referencia = header-posnr.
      ELSEIF header-referencia IS NOT INITIAL.
        referencia = header-referencia.
      ENDIF.

      l_posnr        = referencia.
      l_editar       = COND #( WHEN acao = 'BNT_EDIT' OR acao = 'BNT_QTD' THEN abap_true
                                                                                ELSE abap_false ).

*-CS2023000189-19.04.2023-#108709-JT-inicio
      CALL FUNCTION 'ZSD_SELECAO_FORMACAO_LOTE'
        EXPORTING
          i_nro_sol_ov = lv_nro_sol_ov
          i_contrato   = lv_bstkd
          i_referencia = referencia
          i_posnr      = l_posnr
          i_editar     = l_editar
        IMPORTING
          e_back       = l_back
        TABLES
          t_retorno    = t_0328.

      TRY .
          seq      = t_0328[ 1 ]-zseq_inst.
          referencia = referencia.
        CATCH cx_sy_itab_line_not_found.
          CLEAR seq.
      ENDTRY.

      IF l_back = abap_true.
        CLEAR: seq.
        EXIT.
      ENDIF.

      IF acao = 'BNT_EDIT' OR acao = 'BNT_QTD'.
        IF t_0328[] IS NOT INITIAL.
          READ TABLE t_0066 INTO DATA(w_0066) INDEX value_edit.
          CLEAR: header-volum, header-zmeng,
                 w_0066-volum, w_0066-zmeng.

          LOOP AT t_0328 INTO DATA(w_zsdt0328) WHERE referencia = referencia.
            header-volum       = w_0066-volum       + w_zsdt0328-quantidade.
            header-zmeng       = w_0066-zmeng       + w_zsdt0328-btgew.
            w_0066-volum       = w_0066-volum       + w_zsdt0328-quantidade.
            w_0066-zmeng       = w_0066-zmeng       + w_zsdt0328-btgew.
          ENDLOOP.
        ENDIF.
        MODIFY t_0066 FROM w_0066  INDEX value_edit.
      ENDIF.

*-CS2023000189-04.09.2023-#122555-JT-inicio
      IF acao = 'BNT_EDIT'. "*-CS2023000189-04.09.2023-#122555-JT
        CLEAR: seq.
      ENDIF.

      IF acao = 'BNT_QTD'.
        CLEAR: seq.

        me->complementa_dados_header( CHANGING header = header ).

        TRY .
            me->check_zmeng( EXPORTING header = header
                             CHANGING  t_0066 = t_0066 ).

            CHECK acao EQ 'BNT_QTD'.

            APPEND CORRESPONDING #( header ) TO t_alt_qtd.

          CATCH zcl_cx_excecoes_cad_form_lote.

        ENDTRY.

      ENDIF.

    ELSE.

      CALL FUNCTION 'ZSD_SELECAO_FORMACAO_LOTE_BKGR'
        TABLES
          t_retorno = t_instrucoes.

    ENDIF.

  ENDMETHOD.


  METHOD calcula_lote.
    DATA: lv_menge  TYPE ekpo-menge,
          lv_matnr  TYPE matnr18,
          lv_matnr2 TYPE mara-matnr.

    IF header-matnr IS NOT INITIAL AND
       header-zieme IS NOT INITIAL AND
       header-pmein IS NOT INITIAL AND
       header-zmeng IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = header-matnr
        IMPORTING
          output = lv_matnr.

      lv_matnr2 = lv_matnr.

      CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
        EXPORTING
          i_matnr              = lv_matnr2
          i_in_me              = header-zieme
          i_out_me             = header-pmein
          i_menge              = header-zmeng
        IMPORTING
          e_menge              = lv_menge
        EXCEPTIONS
          error_in_application = 1
          error                = 2
          OTHERS               = 3.
      IF sy-subrc IS INITIAL.
        vlrtot =  lv_menge * header-dmbtr.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD calcula_vlr_usd.
    CONSTANTS lc_usd(5)   TYPE p DECIMALS 4 VALUE'22.046'.

    header-usd_to = header-libra_to.

    MULTIPLY header-usd_to BY lc_usd.

    DATA(lv_qtd) = me->calcula_lote( header ).

    CASE header-zieme.
      WHEN 'TO'.
        header-vlr_tot_frm_usd = header-usd_to.
        MULTIPLY header-vlr_tot_frm_usd BY lv_qtd.
      WHEN OTHERS.
        header-vlr_tot_frm_usd = ( header-usd_to * lv_qtd ) / 1000.
    ENDCASE.

  ENDMETHOD.


  METHOD check_botao.

*    Verifica se existe Algum Vbeln Preenchido Se Existir Exibe o Botão ALterar Qtd
    DATA(qtd) = REDUCE i( INIT x = 0 FOR ls IN t_0066
                        WHERE ( vbeln IS NOT INITIAL )
                            NEXT x = x + 1 ).

    IF qtd IS INITIAL.

      LOOP AT SCREEN.
        CASE screen-name.
          WHEN 'BNT_QTD'.
            screen-invisible = 1.
            MODIFY SCREEN.
          WHEN OTHERS.

            CHECK acao CS 'BNT_'.

            IF screen-name CS 'BNT_' AND
               screen-name NE acao .
              screen-input = 0.
              MODIFY SCREEN.
            ENDIF.

        ENDCASE.
      ENDLOOP.

    ELSE.
      CHECK acao CS 'BNT_'.

      LOOP AT SCREEN.
        IF screen-name CS 'BNT_' AND
           screen-name NE acao .
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD check_zmeng.

    DATA: l_total        TYPE vbfa-rfmng,
          ls_SCX_T100KEY TYPE scx_t100key.

    FREE: l_total.

    SELECT *
      FROM vbfa
      INTO TABLE @DATA(t_vbfa)
     WHERE vbelv   = @header-vbeln
       AND vbtyp_n = 'J'
       AND vbtyp_v = 'C'.

    IF sy-subrc = 0.
      LOOP AT t_vbfa INTO DATA(w_vbfa).
        l_total = l_total + w_vbfa-rfmng.
      ENDLOOP.

      IF header-zmeng < l_total.


        ls_SCX_T100KEY-msgid = 'ZSD'.
        ls_SCX_T100KEY-msgno = '024'.
        ls_SCX_T100KEY-attr1 = 'Quantidade da Formação de Lote'.
        ls_SCX_T100KEY-attr2 = 'é menor'.
        ls_SCX_T100KEY-attr3 = 'que total de Remessas Geradas: '.
        ls_SCX_T100KEY-attr4 = l_total.

        RAISE EXCEPTION TYPE zcl_cx_excecoes_cad_form_lote EXPORTING textid = ls_SCX_T100KEY.

      ENDIF.

    ENDIF.

    LOOP AT t_0066 ASSIGNING FIELD-SYMBOL(<f0066>)
            WHERE nro_sol_ov EQ header-nro_sol_ov
              AND posnr EQ header-posnr
              AND vbeln IS NOT INITIAL.
      <f0066> = CORRESPONDING #( header ).
    ENDLOOP.

  ENDMETHOD.


  METHOD complementa_dados_header.

    header-werks_desc    = me->busca_descricoes( EXPORTING campo = 'WERKS' CHANGING valor = header-werks ).
    header-matnr_desc    = me->busca_descricoes( EXPORTING campo = 'MATNR' CHANGING valor = header-matnr ).
    header-terminal_desc = me->busca_descricoes( EXPORTING campo = 'TERMINAL' CHANGING valor = header-terminal ).
    header-ponto_c_desc  = me->busca_descricoes( EXPORTING campo = 'PONTO_C' CHANGING valor = header-ponto_c ).
    header-lentrega_desc = me->busca_descricoes( EXPORTING campo = 'LENTREGA' CHANGING valor = header-lentrega ).
    header-kunnr_desc    = me->busca_descricoes( EXPORTING campo = 'KUNNR' CHANGING valor = header-kunnr ).

    header-vlrtot        = me->calcula_lote( header ).
    me->calcula_vlr_usd( CHANGING header = header ).

  ENDMETHOD.


  METHOD constructor.

*    me->freetable( ).

  ENDMETHOD.


  METHOD deleta_lote.

    DATA: lt_dele  TYPE TABLE OF zsdt0066,
          lv_char  TYPE string,
          lv_value TYPE c.

    DATA(lv_item) = t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                                            CHANGING acao = acao  ) ]-posnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = lv_value
      IMPORTING
        output = lv_char.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = |Deseja Deletar o Item { lv_char }?|
        text_button_1         = 'Sim'
        text_button_2         = 'Não'
        display_cancel_button = ' '
      IMPORTING
        answer                = lv_value.

    IF lv_value EQ 1.
      IF t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                                CHANGING acao = acao ) ]-vbeln IS INITIAL.

        t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                               CHANGING acao = acao ) ]-status = 'D'.
        t_0066[ me->get_index(  EXPORTING obj_alv = obj_alv
                               CHANGING acao = acao ) ]-color = 'C600'.

        APPEND CORRESPONDING #( t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                                                       CHANGING acao = acao ) ] ) TO t_dele.
      ELSE.
        MESSAGE |Formação de Lote já possui Ordem Gerada!| TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.



  ENDMETHOD.


  METHOD DESBLOQUEIA_NRO_SOLICITACAO.

    CALL FUNCTION 'DEQUEUE_EZSDT0066'
      EXPORTING
        nro_sol_ov     = nro_solicitacao
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc IS INITIAL.
      sucesso = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD edit_lote.
    DATA: ls_SCX_T100KEY TYPE scx_t100key.

    TRY .
        CHECK me->get_index( EXPORTING obj_alv = obj_alv
                             CHANGING acao = acao
                              ) IS NOT INITIAL.
      CATCH zcl_cx_excecoes_cad_form_lote.

    ENDTRY.

    IF t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                             CHANGING acao = acao  ) ]-vbeln IS NOT INITIAL.

      ls_SCX_T100KEY-msgid = 'SD'.
      ls_SCX_T100KEY-msgno = '836'.
      ls_SCX_T100KEY-attr1 = 'Formação de Lote já possui Ordem Gerada'.

      RAISE EXCEPTION TYPE zcl_cx_excecoes_cad_form_lote EXPORTING textid = ls_SCX_T100KEY.

      CLEAR: header, acao.
      EXIT.
    ENDIF.

    TRY .
        ASSIGN t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                             CHANGING acao = acao ) ] TO FIELD-SYMBOL(<f0066>).
        IF <f0066> IS ASSIGNED.
          <f0066>-color = 'C311'.
          header = <f0066>.
          me->complementa_dados_header( CHANGING header = header ).
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        CLEAR header.
    ENDTRY.

    IF header-posnr IS INITIAL.


      ls_SCX_T100KEY-msgid = 'SD'.
      ls_SCX_T100KEY-msgno = '836'.
      ls_SCX_T100KEY-attr1 = 'Nr. do Documento não pode ser Editado!'.

      RAISE EXCEPTION TYPE zcl_cx_excecoes_cad_form_lote EXPORTING textid = ls_SCX_T100KEY.

    ENDIF.

  ENDMETHOD.


  METHOD exibe_menssagens.

    DATA: lv_campo    TYPE char20,
          lv_campo2   TYPE char20,
          lt_edit1    TYPE TABLE OF zsdt0066,
          lw_mensagem TYPE char30.

    FREE: t_msg_ret.

    CASE acao.
      WHEN 'BNT_ADD'.
        CHECK sy-ucomm NE 'SAVE'.

        LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fcat>) WHERE no_out IS INITIAL.

          lv_campo = |header-{ <fcat>-fieldname }|.
          ASSIGN (lv_campo) TO FIELD-SYMBOL(<fs_campo>).

          CASE  <fcat>-fieldname.
            WHEN
                'MANDT' OR
                'NRO_SOL_OV' OR
                'POSNR' OR
                'VBELN' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'STATUS_FORM' OR
                'USNAM' OR
                'DATA_ATUAL' OR
                'HORA_ATUAL' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'AVISO' OR
                'PONTO_C' OR
                'PONTO_C_DESC' OR
                'DCO' OR
                'ICON' OR
                'ZONA_PC' OR
                'ZONA_LR' OR
                'KVGR4' OR
                'KVGR5'.
            WHEN OTHERS.
              IF <fs_campo> IS INITIAL.

                APPEND VALUE #(
                                field = <fcat>-fieldname
                                msg = |campo { <fcat>-scrtext_l } obrigatório!|
                               ) TO t_msg_ret.
*** BUG 58231 - CSB - Inicio
              ELSE.
                IF <fcat>-fieldname EQ 'LGORT'.

                  lv_campo = |header-{ <fcat>-fieldname }|.
                  ASSIGN (lv_campo) TO <fs_campo>.

                  lv_campo2 = |header-WERKS|.
                  ASSIGN (lv_campo2) TO FIELD-SYMBOL(<fs_campo2>).

                  SELECT SINGLE lgort
                        INTO @DATA(vlgort)
                        FROM t001l
                        WHERE werks EQ @<fs_campo2>
                          AND lgort EQ @<fs_campo> .

                  IF vlgort IS INITIAL.
                    APPEND VALUE #(
                                field = <fcat>-fieldname
                                msg = |campo { <fcat>-scrtext_l } não existe na tabela T001L .|
                               ) TO t_msg_ret.
                  ENDIF.

                ENDIF.
              ENDIF.
*** BUG 58231 - CSB - Fim
          ENDCASE.

        ENDLOOP.

        IF t_msg_ret[] IS INITIAL .
          DATA vol_fardos_66 TYPE zsded029.
          DATA vol_fardos_retirar TYPE volum.

          LOOP AT t_0066 INTO DATA(wa_it_0066) WHERE instrucao = header-instrucao AND werks = header-werks AND posnr IS INITIAL.
            ADD wa_it_0066-volum TO vol_fardos_66.
          ENDLOOP.

          SELECT  SUM( volum )
            FROM zsdt0066
            INTO @DATA(vol_fardos_66_auxi)
           WHERE instrucao = @header-instrucao
           AND   werks   = @header-werks
           AND   status  <> 'D'.

          ADD vol_fardos_66_auxi TO vol_fardos_66.

          ADD header-volum TO  vol_fardos_66.

          SELECT quantidade
            FROM zsdt0045
            INTO TABLE @DATA(it_fardos_45)
           WHERE instrucao = @header-instrucao
           AND    werks    = @header-werks.

          DATA vol_fardos_45 TYPE n LENGTH 10.

          LOOP AT it_fardos_45 INTO DATA(w_0045).
            ADD w_0045-quantidade TO vol_fardos_45.
          ENDLOOP.

          IF vol_fardos_66 >  vol_fardos_45.
            APPEND VALUE #( msg = | Total de Fardos, das Formações de Lote refenciadas a instrução informada, | ) TO t_msg_ret.
            APPEND VALUE #( msg = | está Superior a quantidade de fardos cadastrados para Instrução/Filial !| ) TO t_msg_ret.
          ENDIF.
        ENDIF.

      WHEN 'BNT_EDIT'.

        LOOP AT t_0066 INTO DATA(ln_0066) WHERE vbeln IS INITIAL.
          DATA(tabix) = sy-tabix.
          LOOP AT t_fcat ASSIGNING <fcat> WHERE no_out IS INITIAL.

            CASE <fcat>-fieldname.
              WHEN 'MANDT' OR
                'NRO_SOL_OV' OR
                'POSNR' OR
                'VBELN' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'STATUS_FORM' OR
                'USNAM' OR
                'DATA_ATUAL' OR
                'HORA_ATUAL' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'AVISO' OR
                'PONTO_C' OR
                'PONTO_C_DESC' OR
                'DCO' OR
                'ICON' OR
                'WERKS_DESC' OR
                'MATNR_DESC' OR
                'TERMINAL_DESC' OR
                'PONTO_C_DESC' OR
                'LENTREGA_DESC' OR
                'KUNNR_DESC' OR
                'ZONA_PC' OR
                'ZONA_LR' OR
                'KVGR4' OR
                'KVGR5' OR
                'CHARG_ORI'.
                CONTINUE.
            ENDCASE.

            lv_campo = |LN_0066-{ <fcat>-fieldname }|.
            ASSIGN (lv_campo) TO <fs_campo>.

            IF <fs_campo> IS INITIAL.
              APPEND VALUE #(
                              field = <fcat>-fieldname
                              msg = |campo { <fcat>-scrtext_l } obrigatório! Linha { tabix }.|
                             ) TO t_msg_ret.
*** BUG 58231 - CSB - Inicio
            ELSE.
              IF <fcat>-fieldname EQ 'LGORT'.

                lv_campo = |LN_0066-{ <fcat>-fieldname }|.
                ASSIGN (lv_campo) TO <fs_campo>.

                lv_campo2 = |LN_0066-WERKS|.
                ASSIGN (lv_campo2) TO <fs_campo2>.

                CLEAR: vlgort.
                SELECT SINGLE lgort
                      INTO vlgort
                      FROM t001l
                      WHERE werks EQ <fs_campo2>
                        AND lgort EQ <fs_campo> .

                IF vlgort IS INITIAL.
                  APPEND VALUE #(
                              field = <fcat>-fieldname
                              msg = |campo { <fcat>-scrtext_l } não existe na tabela T001L { tabix }.|
                             ) TO t_msg_ret.
                ENDIF.

              ENDIF.
            ENDIF.
*** BUG 58231 - CSB - Fim
          ENDLOOP.
        ENDLOOP.

        IF t_msg_ret[] IS INITIAL .

*//////   LOCALIZA O REGISTRO QUE ESTÁ SENDO EDITADO
          lt_edit1 =
                VALUE #( FOR ls_0066 IN t_0066 WHERE ( color EQ 'C311' )
              ( CORRESPONDING #( ls_0066 ) )
            ).
*/////
          READ TABLE lt_edit1 INTO DATA(wa_edit1) INDEX 1.

          SELECT  SUM( volum )
            FROM zsdt0066
            INTO vol_fardos_66_auxi
           WHERE instrucao = wa_edit1-instrucao
           AND   werks   = wa_edit1-werks
           AND   status  <> 'D'.

          ADD vol_fardos_66_auxi TO vol_fardos_66.

*//////  Totaliza o Valor Original dessa instrução
          LOOP AT lt_edit1 INTO DATA(w_edit1).

            SELECT SUM( volum )
              FROM zsdt0066
              INTO @DATA(vl_fd_ret)
             WHERE nro_sol_ov = @w_edit1-nro_sol_ov
             AND   posnr   = @w_edit1-posnr.

            ADD vl_fd_ret TO vol_fardos_retirar.
          ENDLOOP.
*/////

          vol_fardos_66 = vol_fardos_66 - vol_fardos_retirar.



          ADD header-volum TO  vol_fardos_66.

          SELECT quantidade
            FROM zsdt0045
            INTO TABLE it_fardos_45
           WHERE instrucao = header-instrucao
           AND    werks    = header-werks.

          LOOP AT it_fardos_45 INTO w_0045.
            ADD w_0045-quantidade TO vol_fardos_45.
          ENDLOOP.

          IF vol_fardos_66 >  vol_fardos_45.
            APPEND VALUE #( msg = | Total de Fardos, das Formações de Lote refenciadas a instrução informada, | ) TO t_msg_ret.
            APPEND VALUE #( msg = | está Superior a quantidade de fardos cadastrados para Instrução/Filial !| ) TO t_msg_ret.
          ENDIF.
        ENDIF.

    ENDCASE.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_repid     = sy-repid
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = texto_bt_dinamico
      TABLES
        it_msgs     = t_msg_ret.

    IF t_msg_ret[] IS NOT INITIAL AND sy-ucomm NE 'SHOW_MSGRE'.

      MESSAGE 'Verifique o Log de Erros!' TYPE 'S' DISPLAY LIKE 'E'.

    ELSE.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen    = '100'
          i_show      = abap_true
          i_repid     = sy-repid
          i_popup     = 1
          i_set_field = 'X_FIELD'
        IMPORTING
          e_messagem  = texto_bt_dinamico
        TABLES
          it_msgs     = t_msg_ret.

    ENDIF.

  ENDMETHOD.


  METHOD get_0045.

    DATA: l_quant TYPE i.

    CLEAR l_quant.

    IF seq IS NOT INITIAL.
      SELECT SINGLE * FROM zsdt0045
            INTO @DATA(wa_0045)
            WHERE zseq_inst EQ @seq.
    ENDIF.

    IF  instrucao IS NOT INITIAL.
      SELECT COUNT(*) FROM zsdt0045
        INTO @DATA(qtd)
         WHERE instrucao EQ @instrucao.
      IF qtd EQ 1.

        SELECT SINGLE * FROM zsdt0045
          INTO wa_0045
         WHERE instrucao EQ instrucao.

      ENDIF.
    ENDIF.

    CHECK wa_0045 IS NOT INITIAL.

    header            = CORRESPONDING #( wa_0045 ).
    header-referencia = referencia.  "*-CS2023000189-19.04.2023-#108709-JT

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF t_0328[] IS NOT INITIAL.
      CLEAR: wa_0045-quantidade, wa_0045-btgew.
      LOOP AT t_0328 INTO DATA(w_zsdt0328) WHERE referencia = header-referencia.
        l_quant            = l_quant            + 1.
        wa_0045-quantidade = wa_0045-quantidade + w_zsdt0328-quantidade.
        wa_0045-btgew      = wa_0045-btgew      + w_zsdt0328-btgew.
      ENDLOOP.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

    header-instrucao   = wa_0045-instrucao.
    header-charg       = wa_0045-safra.
    header-volum       = wa_0045-quantidade.
    header-nro_sol_ov  = wa_0045-objek.
    header-kunnr       = header-werks.
    header-charg_ori   = COND #( WHEN l_quant  > 1 THEN '*'  "*-CS2023000189-19.04.2023-#108709-JT-inicio
                                                   ELSE wa_0045-charg ).
    header-ponto_c     = wa_0045-ponto_c.
    header-zmeng       = wa_0045-btgew.
    header-lentrega    = me->get_local_entrega( wa_0045-terminal_estuf ).
    header-terminal    = wa_0045-terminal_estuf.
    header-zieme       = wa_0045-gewei.

    CLEAR: header-dmbtr,  header-pmein.

  ENDMETHOD.


  method GET_DADOS_TRACE.

DATA: lt_zsdt0327 TYPE tb_0327.

  IF t_0066[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0327
        INTO TABLE lt_zsdt0327
         FOR ALL ENTRIES IN t_0066
       WHERE nro_sol_ov  = t_0066-nro_sol_ov
         AND posnr       = t_0066-posnr.
    ENDIF.

    SORT lt_zsdt0327 BY nro_sol_ov posnr seq DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_zsdt0327
                     COMPARING nro_sol_ov posnr.
*-CS2023000189-26.04.2023-#108710-JT-fim

    LOOP AT t_0066 ASSIGNING FIELD-SYMBOL(<f0066>).

      READ TABLE lt_zsdt0327 INTO DATA(w_zsdt0327) WITH KEY nro_sol_ov  = <f0066>-nro_sol_ov
                                                     posnr       = <f0066>-posnr.
      IF sy-subrc <> 0.
        <f0066>-status_trace = icon_dummy.
      ELSEIF w_zsdt0327-tipo_msg = 'E'.
        <f0066>-status_trace = icon_alert. "ICON_FAILURE
      ELSEIF w_zsdt0327-tipo_msg = 'S'.
        <f0066>-status_trace = icon_checked.
      ELSE.
        <f0066>-status_trace = icon_dummy.
      ENDIF.

    ENDLOOP.

  endmethod.


  METHOD get_index.
    DATA: lt_index       TYPE lvc_t_row,
          l_value_edit   TYPE sy-tabix,
          ls_SCX_T100KEY TYPE scx_t100key.

    CALL METHOD obj_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_index.

    CASE acao.
      WHEN 'BNT_LTO1' OR 'BNT_QTD'.

        IF lt_index[] IS INITIAL.

          ls_SCX_T100KEY-msgid = 'SD'.
          ls_SCX_T100KEY-msgno = '836'.
          ls_SCX_T100KEY-attr1 = 'Selecione ao menos uma Linha'.

          RAISE EXCEPTION TYPE zcl_cx_excecoes_cad_form_lote EXPORTING textid = ls_SCX_T100KEY.

          CLEAR acao.
          EXIT.
        ENDIF.

        index      = lt_index[ 1 ]-index.
        l_value_edit = index.

      WHEN 'BNT_TRACE'.
        IF lines( lt_index[] ) EQ 1.
          index = lt_index[ 1 ].
        ELSE.
          FREE: lt_index[].

          ls_SCX_T100KEY-msgid = 'SD'.
          ls_SCX_T100KEY-msgno = '836'.
          ls_SCX_T100KEY-attr1 = 'Selecione Somente uma Linha para Reenvio!'.

          RAISE EXCEPTION TYPE zcl_cx_excecoes_cad_form_lote EXPORTING textid = ls_SCX_T100KEY.

        ENDIF.

      WHEN OTHERS.

        IF lines( lt_index[] ) EQ 1.
          index      = lt_index[ 1 ].
          l_value_edit = index.
        ELSE.
          FREE: lt_index[].

          ls_SCX_T100KEY-msgid = 'SD'.
          ls_SCX_T100KEY-msgno = '836'.
          ls_SCX_T100KEY-attr1 = 'Selecione Somente uma Linha para Edição!'.

          RAISE EXCEPTION TYPE zcl_cx_excecoes_cad_form_lote EXPORTING textid = ls_SCX_T100KEY.

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  method GET_LOCAL_ENTREGA.

    CHECK lifnr IS NOT INITIAL.

    SELECT SINGLE kn~kunnr
      FROM lfa1 AS lf
      INNER JOIN kna1 AS kn  ON lf~stcd1 = kn~stcd1
    "  INTO TABLE @DATA(TB)
      INTO local_entrega
    WHERE lf~lifnr = lifnr.

  endmethod.


  METHOD get_posnr.

    SELECT MAX( posnr )
     FROM zsdt0066
     INTO posnr
     WHERE nro_sol_ov EQ nro_solicitacao.

    DATA(seq) = REDUCE i( INIT x = 0 FOR ls_save IN t_save WHERE ( posnr IS NOT INITIAL ) NEXT x = x + 1 ).

    seq = seq * 10.
    ADD seq TO posnr.
    ADD 10 TO posnr.

*-CS2023000189-19.04.2023-#108709-JT-inicio
    LOOP AT t_0328   INTO DATA(w_zsdt0328) WHERE referencia = referencia.
      w_zsdt0328-nro_sol_ov = nro_solicitacao.
      w_zsdt0328-posnr      = posnr.
      MODIFY t_0328  FROM w_zsdt0328 INDEX sy-tabix.
    ENDLOOP.
*-CS2023000189-19.04.2023-#108709-JT-fim

  ENDMETHOD.


  METHOD get_seq.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = id
      IMPORTING
        number      = r_value.

  ENDMETHOD.


  METHOD grava_log.

    DATA: tabix       TYPE sy-tabix,
          lt_fcat_log TYPE lvc_t_fcat,
          lo_str      TYPE REF TO data,
          wa_0066_old TYPE ty_0066.

    FREE lt_fcat_log.

    ASSIGN 'ZSDT0066' TO FIELD-SYMBOL(<fs_str>).
    CREATE DATA lo_str TYPE (<fs_str>).

    lt_fcat_log = CORRESPONDING lvc_t_fcat( cl_salv_data_descr=>read_structdescr( CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( lo_str ) ) ) ).

    LOOP AT t_0066 INTO DATA(wa_0066).

      tabix = sy-tabix.
      CLEAR wa_0066_old.

      TRY .
          wa_0066_old = t_0066_old[ tabix ].
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      LOOP AT lt_fcat_log INTO DATA(wa_fcat).
        me->input_log( EXPORTING
                        nro_solicitacao = nro_solicitacao
                        value1 = |WA_0066-{ wa_fcat-fieldname }|
                        value2 = 'form.lote'
                        value3 = tabix
                     ).
      ENDLOOP.
    ENDLOOP.

    FREE lt_fcat_log.

  ENDMETHOD.


  METHOD input_log.

    DATA: wl_field(30),
          wl_field_old(40),
          wl_field_aux(40),
          wl_field_aux2(40),
          lt_save_log     TYPE TABLE OF zsdt0083.

    FIELD-SYMBOLS: <fs_field>     TYPE any,
                   <fs_field_old> TYPE any.

    UNASSIGN <fs_field>.
    UNASSIGN <fs_field_old>.

    wl_field = value1.
    SPLIT wl_field AT '-' INTO wl_field_aux
                               wl_field_aux2.

    wl_field_old = |{ wl_field_aux }_OLD-{ wl_field_aux2 }| .

    ASSIGN (wl_field) TO <fs_field>.
    ASSIGN (wl_field_old) TO <fs_field_old>.
    IF <fs_field> IS ASSIGNED.

      IF <fs_field> NE <fs_field_old>.

        IF  me->at_hist IS INITIAL.

          me->at_hist = me->get_seq( EXPORTING id = 'ZHISTORIC').

        ENDIF.

        SPLIT value1 AT '-' INTO wl_field
                                 wl_field_aux.

        APPEND VALUE #( nro_sol_ov   = nro_solicitacao
                        linha        = value3
                        id_historico = me->at_hist
                        area         = value2
                        campo        = wl_field_aux
                        new_value    = <fs_field>
                        old_value    = <fs_field_old>
                        usnam        = sy-uname
                        data_atual   = sy-datum
                        hora_atual   = sy-uzeit
                     ) TO lt_save_log.

        MODIFY zsdt0083 FROM TABLE lt_save_log[].

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD liberar_lote.

    IF background IS INITIAL.

      CHECK NOT  me->get_index( EXPORTING obj_alv = obj_alv
                                CHANGING acao     = acao ) IS INITIAL.

      DATA(wa_del) = t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                                             CHANGING acao     = acao ) ].

      CHECK wa_del-nro_sol_ov IS NOT INITIAL
              OR wa_del-posnr IS NOT INITIAL
          OR wa_del-instrucao IS NOT INITIAL.

    ELSE.

      READ TABLE t_0066 INTO wa_del INDEX 1.

    ENDIF.

    UPDATE zsdt0066 SET status = 'L'
           WHERE nro_sol_ov EQ wa_del-nro_sol_ov
                  AND posnr EQ wa_del-posnr
              AND instrucao EQ wa_del-instrucao.
    IF sy-subrc IS INITIAL.
      mensagem = |Solicitação { wa_del-nro_sol_ov } - { wa_del-posnr } Liberada!|.

      IF background IS INITIAL.
        me->seleciona_dados_gerais( EXPORTING nro_solicitacao = nro_solicitacao ).
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD modifica_alv.

    CHECK acao CS 'EDIT'.

    LOOP AT t_0066 ASSIGNING FIELD-SYMBOL(<f0066>) WHERE posnr EQ header-posnr.
      CHECK NOT <f0066>-posnr IS INITIAL.
      MOVE-CORRESPONDING header TO <f0066>.
    ENDLOOP.

  ENDMETHOD.


  METHOD monta_matchcode.

    TYPES: BEGIN OF ty_bstkd,
             contrato TYPE zsdt0143-contrato,
             safra    TYPE zsdt0143-safra,
             empresa  TYPE zsdt0143-empresa,
           END OF ty_bstkd,

           BEGIN OF ty_solov,
             nro_sol_ov TYPE zsdt0051-nro_sol_ov,
             vkbur      TYPE zsdt0051-vkbur,
             auart      TYPE zsdt0051-auart,
             inco1      TYPE zsdt0051-inco1,
             matnr      TYPE zsdt0051-matnr,
           END OF ty_solov.

    DATA: it_return TYPE TABLE OF ddshretval,
          tl_dselc  TYPE TABLE OF dselc.

    CASE campo.
      WHEN 'CONTRATO'.

        DATA: it_bstkd TYPE TABLE OF ty_bstkd.

        SELECT contrato safra empresa
          FROM zsdt0143
          INTO TABLE it_bstkd
        WHERE cancelado EQ abap_false.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CONTRATO'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            value_org       = 'S'
          TABLES
            value_tab       = it_bstkd
            return_tab      = it_return
            dynpfld_mapping = tl_dselc.

        TRY .
            valor = it_return[ 1 ]-fieldval.
          CATCH cx_sy_itab_line_not_found.
            CLEAR valor.
        ENDTRY.

      WHEN 'NRO_SOL_OV'.

        DATA: it_solov TYPE  TABLE OF ty_solov.

        SELECT nro_sol_ov vkbur auart inco1 matnr
          FROM zsdt0051
          INTO TABLE it_solov
          WHERE param_espec EQ 'A'.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'NRO_SOL_OV'
            dynpprog        = sy-repid
            dynpnr          = sy-dynnr
            value_org       = 'S'
          TABLES
            value_tab       = it_solov
            return_tab      = it_return
            dynpfld_mapping = tl_dselc.

        TRY .
            valor = it_return[ 1 ]-fieldval.
          CATCH cx_sy_itab_line_not_found.
            CLEAR valor.
        ENDTRY.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD reenvia_trace.

    DATA: l_nro_sol_ov TYPE zsdt0066-nro_sol_ov,
          l_posnr      TYPE zsdt0066-posnr,
          lv_acao      TYPE sy-ucomm.

    lv_acao = 'BNT_TRACE'.

    CHECK me->get_index( EXPORTING obj_alv = obj_alv
                         CHANGING acao = lv_acao ) IS NOT INITIAL.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 50
        text       = 'Aguarde...Integrando...'.

    CHECK t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                                  CHANGING acao = lv_acao ) ]-vbeln IS NOT INITIAL.

    CLEAR lv_acao.

    l_nro_sol_ov = t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                                          CHANGING acao = lv_acao ) ]-nro_sol_ov.
    l_posnr      = t_0066[ me->get_index( EXPORTING obj_alv = obj_alv
                                          CHANGING acao = lv_acao ) ]-posnr.

    DATA(l_task) = 'TRACE_ORDEM_VENDA' && l_nro_sol_ov && l_posnr.

    CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE' STARTING NEW TASK l_task
      EXPORTING
        i_nro_sol_ov = l_nro_sol_ov
        i_posnr      = l_posnr
        i_acao       = 'C'
      EXCEPTIONS
        OTHERS       = 1.

  ENDMETHOD.


  METHOD save.

    DATA: lt_0066 TYPE TABLE OF zsdt0066,
          lt_save TYPE TABLE OF zsdt0066,
          wa_save TYPE zsdt0066,
          lt_edit TYPE TABLE OF zsdt0066,
          lt_dele TYPE TABLE OF zsdt0066,
          l_tabix TYPE sy-tabix.

    IF background IS INITIAL.
      me->set_erros( EXPORTING acao      = acao
                               t_0066    = t_0066
                               t_fcat    = t_fcat
                      CHANGING header    = header
                               t_msg_ret = t_msg_ret ).

      CHECK t_msg_ret[] IS INITIAL.

    ENDIF.

    LOOP AT t_0066 INTO  DATA(ws_0066) WHERE posnr IS INITIAL.
      ws_0066-posnr = me->get_posnr( EXPORTING nro_solicitacao = nro_solicitacao
                                               referencia = ws_0066-referencia
                                               t_save = lt_save[]
                                      CHANGING t_0328 = t_0328[] ).
      MOVE-CORRESPONDING ws_0066 TO wa_save.
      APPEND wa_save TO lt_save.
    ENDLOOP.

    lt_edit =
      VALUE #( FOR ls_0066 IN t_0066 WHERE ( color EQ 'C311' )
               ( CORRESPONDING #( ls_0066 ) )
             ).

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF lt_edit[] IS NOT INITIAL.
      LOOP AT lt_edit INTO DATA(wa_edit).
        LOOP AT t_0328   INTO DATA(w_zsdt0328) WHERE referencia = wa_edit-posnr.
          w_zsdt0328-nro_sol_ov = wa_edit-nro_sol_ov.
          w_zsdt0328-posnr      = wa_edit-posnr.
          MODIFY t_0328  FROM w_zsdt0328 INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

    DELETE lt_save WHERE posnr IS INITIAL.
    DELETE lt_dele WHERE posnr IS INITIAL.

    APPEND LINES OF lt_save[] TO lt_0066[].
    APPEND LINES OF lt_edit[] TO lt_0066[].

*-CS2023000189-19.04.2023-#108709-JT-inicio
    LOOP AT lt_0066        INTO DATA(wg_0066) WHERE charg_ori = '*'.
      l_tabix = sy-tabix.
      READ TABLE t_0328 INTO w_zsdt0328 WITH KEY nro_sol_ov = wg_0066-nro_sol_ov
                                                     posnr      = wg_0066-posnr.
      wg_0066-charg_ori       = w_zsdt0328-charg_ori.
      MODIFY lt_0066       FROM wg_0066 INDEX l_tabix TRANSPORTING charg_ori.
    ENDLOOP.
*-CS2023000189-19.04.2023-#108709-JT-fim

    IF NOT lt_dele IS INITIAL.

      MODIFY zsdt0066 FROM TABLE lt_dele.

*-CS2023000189-04.09.2023-#122555-JT-inicio
      LOOP AT lt_dele INTO DATA(w_dele).
        UPDATE zsdt0328 SET cancelado  = abap_true
                      WHERE nro_sol_ov = w_dele-nro_sol_ov
                        AND posnr      = w_dele-posnr.
      ENDLOOP.
*-CS2023000189-04.09.2023-#122555-JT-fim

      IF sy-subrc IS INITIAL.
        IF lines( lt_dele[] ) > 1.
          MESSAGE |Documentos Deletados com Sucesso!| TYPE 'S'.
        ELSE.
          MESSAGE |Documento Deletado com Sucesso!| TYPE 'S'.
        ENDIF.
      ELSE.
        MESSAGE |Documentos não foram Deletados!| TYPE 'S' DISPLAY LIKE 'W'.
        EXIT.
      ENDIF.
      FREE lt_dele[].
      WAIT UP TO 2 SECONDS.

      IF lt_0066[] IS INITIAL.
        me->seleciona_dados_gerais( EXPORTING nro_solicitacao = nro_solicitacao
                                    IMPORTING t_0066 = t_0066
                                              t_0328 = t_0328
                                              t_0327 = t_0327
                                              header = header ).
      ENDIF.

    ENDIF.

    IF t_alt_qtd[] IS NOT INITIAL.
      lt_0066[] = t_alt_qtd[].
      CHECK me->atualiza_qtd( lt_0066[] ) IS INITIAL.
    ENDIF.

*   CHECK lt_0066[] IS NOT INITIAL.

    IF lt_0066[] IS INITIAL.
      MESSAGE s024(sd) WITH 'Lote não foi Alterado!' DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    MODIFY zsdt0066 FROM TABLE lt_0066.

    IF sy-subrc IS INITIAL.
*-CS2023000189-19.04.2023-#108709-JT-inicio
      LOOP AT lt_0066 INTO wg_0066.
        LOOP AT t_0328   INTO w_zsdt0328 WHERE referencia = wg_0066-posnr.
          w_zsdt0328-nro_sol_ov = wg_0066-nro_sol_ov.
          w_zsdt0328-posnr      = wg_0066-posnr.
          MODIFY t_0328  FROM w_zsdt0328 INDEX sy-tabix.
        ENDLOOP.
      ENDLOOP.

      LOOP AT lt_0066         INTO wg_0066.
        UPDATE zsdt0328 SET cancelado  = abap_true  "*-CS2023000189-04.09.2023-#122555-JT
                      WHERE nro_sol_ov = wg_0066-nro_sol_ov
                        AND posnr      = wg_0066-posnr.
*       DELETE FROM zsdt0328 WHERE nro_sol_ov = wg_0066-nro_sol_ov  "*-CS2023000189-04.09.2023-#122555-JT
*                              AND posnr      = wg_0066-posnr.
      ENDLOOP.

      LOOP AT lt_0066      INTO wg_0066.
        LOOP AT t_0328 INTO w_zsdt0328 WHERE nro_sol_ov = wg_0066-nro_sol_ov
                                             AND posnr      = wg_0066-posnr.
          MODIFY zsdt0328  FROM w_zsdt0328.
        ENDLOOP.
      ENDLOOP.

      COMMIT WORK.

*-CS2023000189-19.04.2023-#108709-JT-fim

*-CS2023000189-#126959-07.11.2023-JT-inicio
*-------------------------------------------
*-envia trace cotton
*-------------------------------------------

      IF background IS INITIAL.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 50
            text       = 'Aguarde...Integrando...'.
      ENDIF.

      LOOP AT lt_0066 INTO wg_0066.
        CALL FUNCTION 'ZSD_ENVIO_ORDEM_VENDA_TRACE'
          EXPORTING
            i_nro_sol_ov = wg_0066-nro_sol_ov
            i_posnr      = wg_0066-posnr
            i_acao       = 'C'
          EXCEPTIONS
            OTHERS       = 1.
      ENDLOOP.
*-CS2023000189-#126959-07.11.2023-JT-fim

      me->grava_log( EXPORTING nro_solicitacao = nro_solicitacao
                               t_0066 = lt_0066 ).

      MESSAGE |Documentos Salvos com Sucesso!| TYPE 'S'.
*      obj_fmlot->freetable( ).


      me->seleciona_dados_gerais( EXPORTING nro_solicitacao = nro_solicitacao
                                  IMPORTING t_0066 = t_0066
                                            t_0328 = t_0328
                                            t_0327 = t_0327
                                            header = header ).
      IF background IS NOT INITIAL.

        DELETE t_0066 WHERE posnr <> ws_0066-posnr.
        DELETE t_0328 WHERE posnr <> ws_0066-posnr.

      ENDIF.

    ENDIF.

    FREE: lt_dele, lt_0066, t_alt_qtd[].

  ENDMETHOD.


  METHOD seleciona_dados_gerais.
    DATA: ls_SCX_T100KEY TYPE scx_t100key.

    DATA(lw_0051) = me->busca_dados_header( nro_solicitacao = nro_solicitacao ).

    IF lw_0051-param_espec NE 'A' AND
       lw_0051-param_espec NE 'Z'.

      ls_SCX_T100KEY-msgid = 'ZSD'.
      ls_SCX_T100KEY-msgno = '022'.

      RAISE EXCEPTION TYPE zcl_cx_excecoes_cad_form_lote EXPORTING textid = ls_SCX_T100KEY.
    ENDIF.

    SELECT *
      FROM zsdt0066
      INTO CORRESPONDING FIELDS OF TABLE t_0066
      WHERE nro_sol_ov EQ nro_solicitacao
      AND status NE 'D'.


    header-classificacao = 'C'.

*-CS2023000189-26.04.2023-#108710-JT-inicio
    IF t_0066[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0327
        INTO TABLE t_0327
         FOR ALL ENTRIES IN t_0066
       WHERE nro_sol_ov  = t_0066-nro_sol_ov
         AND posnr       = t_0066-posnr.
    ENDIF.

    SORT t_0327 BY nro_sol_ov posnr seq DESCENDING.
    DELETE ADJACENT DUPLICATES FROM t_0327
                     COMPARING nro_sol_ov posnr.
*-CS2023000189-26.04.2023-#108710-JT-fim

    LOOP AT t_0066 ASSIGNING FIELD-SYMBOL(<f0066>).

*-CS2023000189-26.04.2023-#108710-JT-inicio
      READ TABLE t_0327 INTO DATA(w_zsdt0327) WITH KEY nro_sol_ov  = <f0066>-nro_sol_ov
                                                     posnr       = <f0066>-posnr.
      IF sy-subrc <> 0.
        <f0066>-status_trace = icon_dummy.
      ELSEIF w_zsdt0327-tipo_msg = 'E'.
        <f0066>-status_trace = icon_alert. "ICON_FAILURE
      ELSEIF w_zsdt0327-tipo_msg = 'S'.
        <f0066>-status_trace = icon_checked.
      ELSE.
        <f0066>-status_trace = icon_dummy.
      ENDIF.
*-CS2023000189-26.04.2023-#108710-JT-fim

      <f0066>-icon = SWITCH #( <f0066>-status
                               WHEN 'A' OR '' THEN '@Q3@'
                               WHEN 'L'       THEN '@5Y@'
                               WHEN 'E'       THEN '@16@'
                             ).

      <f0066>-werks_desc    = me->busca_descricoes( EXPORTING campo = 'WERKS' CHANGING valor = <f0066>-werks ).
      <f0066>-matnr_desc    = me->busca_descricoes( EXPORTING campo = 'MATNR' CHANGING valor = <f0066>-matnr ).
      <f0066>-terminal_desc = me->busca_descricoes( EXPORTING campo = 'TERMINAL' CHANGING valor = <f0066>-terminal ).
      <f0066>-ponto_c_desc  = me->busca_descricoes( EXPORTING campo = 'PONTO_C' CHANGING valor = <f0066>-ponto_c ).
      <f0066>-lentrega_desc = me->busca_descricoes( EXPORTING campo = 'LENTREGA' CHANGING valor = <f0066>-lentrega ).
      <f0066>-kunnr_desc    = me->busca_descricoes( EXPORTING campo = 'KUNNR' CHANGING valor = <f0066>-kunnr ).

    ENDLOOP.

*-CS2023000189-19.04.2023-#108709-JT-inicio
    IF t_0066[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0328
        INTO TABLE t_0328
         FOR ALL ENTRIES IN t_0066
       WHERE nro_sol_ov = t_0066-nro_sol_ov
         AND posnr      = t_0066-posnr
         AND cancelado  = abap_off.  "*-CS2023000189-04.09.2023-#122555-JT

      LOOP AT t_0328  ASSIGNING FIELD-SYMBOL(<fs_0328>).
        <fs_0328>-referencia = <fs_0328>-posnr.
*        MODIFY t_zsdt0328  FROM w_zsdt0328 TRANSPORTING referencia.
      ENDLOOP.
    ENDIF.
*-CS2023000189-19.04.2023-#108709-JT-fim

*    APPEND LINES OF t_0066[] TO it_0066[].

*    obj_fmlot->set_desc( ).
*    obj_fmlot->monta_log( ).

    me->t_0066_old = t_0066.

  ENDMETHOD.


  METHOD set_erros.

    DATA: lv_campo    TYPE char20,
          lv_campo2   TYPE char20,
          lt_edit1    TYPE TABLE OF zsdt0066,
          lw_mensagem TYPE char30.

    FREE: t_msg_ret.

    CASE acao.
      WHEN 'BNT_ADD'.
        CHECK sy-ucomm NE 'SAVE'.

        LOOP AT t_fcat ASSIGNING FIELD-SYMBOL(<fcat>) WHERE no_out IS INITIAL.

          lv_campo = |header-{ <fcat>-fieldname }|.
          ASSIGN (lv_campo) TO FIELD-SYMBOL(<fs_campo>).

          CASE  <fcat>-fieldname.
            WHEN
                'MANDT' OR
                'NRO_SOL_OV' OR
                'POSNR' OR
                'VBELN' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'STATUS_FORM' OR
                'USNAM' OR
                'DATA_ATUAL' OR
                'HORA_ATUAL' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'AVISO' OR
                'PONTO_C' OR
                'PONTO_C_DESC' OR
                'DCO' OR
                'ICON' OR
                'ZONA_PC' OR
                'ZONA_LR' OR
                'KVGR4' OR
                'KVGR5'.
            WHEN OTHERS.
              IF <fs_campo> IS INITIAL.

                APPEND VALUE #(
                                field = <fcat>-fieldname
                                msg = |lv_campo { <fcat>-scrtext_l } obrigatório!|
                               ) TO t_msg_ret.
*** BUG 58231 - CSB - Inicio
              ELSE.
                IF <fcat>-fieldname EQ 'LGORT'.

                  lv_campo = |header-{ <fcat>-fieldname }|.
                  ASSIGN (lv_campo) TO <fs_campo>.

                  lv_campo2 = |header-WERKS|.
                  ASSIGN (lv_campo2) TO FIELD-SYMBOL(<fs_campo2>).

                  SELECT SINGLE lgort
                        INTO @DATA(vlgort)
                        FROM t001l
                        WHERE werks EQ @<fs_campo2>
                          AND lgort EQ @<fs_campo> .

                  IF vlgort IS INITIAL.
                    APPEND VALUE #(
                                field = <fcat>-fieldname
                                msg = |lv_campo { <fcat>-scrtext_l } não existe na tabela T001L .|
                               ) TO t_msg_ret.
                  ENDIF.

                ENDIF.
              ENDIF.
*** BUG 58231 - CSB - Fim
          ENDCASE.

        ENDLOOP.

        IF t_msg_ret[] IS INITIAL .
          DATA vol_fardos_66 TYPE zsded029.
          DATA vol_fardos_retirar TYPE volum.

          LOOP AT t_0066 INTO DATA(wa_it_0066) WHERE instrucao = header-instrucao AND werks = header-werks AND posnr IS INITIAL.
            ADD wa_it_0066-volum TO vol_fardos_66.
          ENDLOOP.

          SELECT  SUM( volum )
            FROM zsdt0066
            INTO @DATA(vol_fardos_66_auxi)
           WHERE instrucao = @header-instrucao
           AND   werks   = @header-werks
           AND   status  <> 'D'.

          ADD vol_fardos_66_auxi TO vol_fardos_66.

          ADD header-volum TO  vol_fardos_66.

          SELECT quantidade
            FROM zsdt0045
            INTO TABLE @DATA(it_fardos_45)
           WHERE instrucao = @header-instrucao
           AND    werks    = @header-werks.

          DATA vol_fardos_45 TYPE n LENGTH 10.

          LOOP AT it_fardos_45 INTO DATA(w_0045).
            ADD w_0045-quantidade TO vol_fardos_45.
          ENDLOOP.

          IF vol_fardos_66 >  vol_fardos_45.
            APPEND VALUE #( msg = | Total de Fardos, das Formações de Lote refenciadas a instrução informada, | ) TO t_msg_ret.
            APPEND VALUE #( msg = | está Superior a quantidade de fardos cadastrados para Instrução/Filial !| ) TO t_msg_ret.
          ENDIF.
        ENDIF.

      WHEN 'BNT_EDIT'.

        LOOP AT t_0066 INTO DATA(ln_0066) WHERE vbeln IS INITIAL.
          DATA(tabix) = sy-tabix.
          LOOP AT t_fcat ASSIGNING <fcat> WHERE no_out IS INITIAL.

            CASE <fcat>-fieldname.
              WHEN 'MANDT' OR
                'NRO_SOL_OV' OR
                'POSNR' OR
                'VBELN' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'STATUS_FORM' OR
                'USNAM' OR
                'DATA_ATUAL' OR
                'HORA_ATUAL' OR
                'STATUS' OR
                'STATUS_TRACE' OR
                'AVISO' OR
                'PONTO_C' OR
                'PONTO_C_DESC' OR
                'DCO' OR
                'ICON' OR
                'WERKS_DESC' OR
                'MATNR_DESC' OR
                'TERMINAL_DESC' OR
                'PONTO_C_DESC' OR
                'LENTREGA_DESC' OR
                'KUNNR_DESC' OR
                'ZONA_PC' OR
                'ZONA_LR' OR
                'KVGR4' OR
                'KVGR5' OR
                'CHARG_ORI'.
                CONTINUE.
            ENDCASE.

            lv_campo = |LN_0066-{ <fcat>-fieldname }|.
            ASSIGN (lv_campo) TO <fs_campo>.

            IF <fs_campo> IS INITIAL.
              APPEND VALUE #(
                              field = <fcat>-fieldname
                              msg = |lv_campo { <fcat>-scrtext_l } obrigatório! Linha { tabix }.|
                             ) TO t_msg_ret.
*** BUG 58231 - CSB - Inicio
            ELSE.
              IF <fcat>-fieldname EQ 'LGORT'.

                lv_campo = |LN_0066-{ <fcat>-fieldname }|.
                ASSIGN (lv_campo) TO <fs_campo>.

                lv_campo2 = |LN_0066-WERKS|.
                ASSIGN (lv_campo2) TO <fs_campo2>.

                CLEAR: vlgort.
                SELECT SINGLE lgort
                      INTO vlgort
                      FROM t001l
                      WHERE werks EQ <fs_campo2>
                        AND lgort EQ <fs_campo> .

                IF vlgort IS INITIAL.
                  APPEND VALUE #(
                              field = <fcat>-fieldname
                              msg = |lv_campo { <fcat>-scrtext_l } não existe na tabela T001L { tabix }.|
                             ) TO t_msg_ret.
                ENDIF.

              ENDIF.
            ENDIF.
*** BUG 58231 - CSB - Fim
          ENDLOOP.
        ENDLOOP.

        IF t_msg_ret[] IS INITIAL .

*//////   LOCALIZA O REGISTRO QUE ESTÁ SENDO EDITADO
          lt_edit1 =
                VALUE #( FOR ls_0066 IN t_0066 WHERE ( color EQ 'C311' )
              ( CORRESPONDING #( ls_0066 ) )
            ).
*/////
          READ TABLE lt_edit1 INTO DATA(wa_edit1) INDEX 1.

          SELECT  SUM( volum )
            FROM zsdt0066
            INTO vol_fardos_66_auxi
           WHERE instrucao = wa_edit1-instrucao
           AND   werks   = wa_edit1-werks
           AND   status  <> 'D'.

          ADD vol_fardos_66_auxi TO vol_fardos_66.

*//////  Totaliza o Valor Original dessa instrução
          LOOP AT lt_edit1 INTO DATA(w_edit1).

            SELECT SUM( volum )
              FROM zsdt0066
              INTO @DATA(vl_fd_ret)
             WHERE nro_sol_ov = @w_edit1-nro_sol_ov
             AND   posnr   = @w_edit1-posnr.

            ADD vl_fd_ret TO vol_fardos_retirar.
          ENDLOOP.
*/////

          vol_fardos_66 = vol_fardos_66 - vol_fardos_retirar.



          ADD header-volum TO  vol_fardos_66.

          SELECT quantidade
            FROM zsdt0045
            INTO TABLE it_fardos_45
           WHERE instrucao = header-instrucao
           AND    werks    = header-werks.

          LOOP AT it_fardos_45 INTO w_0045.
            ADD w_0045-quantidade TO vol_fardos_45.
          ENDLOOP.

          IF vol_fardos_66 >  vol_fardos_45.
            APPEND VALUE #( msg = | Total de Fardos, das Formações de Lote refenciadas a instrução informada, | ) TO t_msg_ret.
            APPEND VALUE #( msg = | está Superior a quantidade de fardos cadastrados para Instrução/Filial !| ) TO t_msg_ret.
          ENDIF.
        ENDIF.

    ENDCASE.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen    = '100'
        i_repid     = sy-repid
        i_set_field = 'X_FIELD'
      IMPORTING
        e_messagem  = texto_bt_dinamico
      TABLES
        it_msgs     = t_msg_ret.

    CHECK t_msg_ret[] IS NOT INITIAL AND sy-ucomm NE 'SHOW_MSGRE'.

    MESSAGE 'Verifique o Log de Erros!' TYPE 'S' DISPLAY LIKE 'E'.

  ENDMETHOD.


  METHOD set_layout.

    CLEAR: layout, variant, stable.

    layout = VALUE #(
                        zebra      = abap_true
                        no_rowins  = abap_true
                        stylefname = 'ESTILO'
                        info_fname = 'COLOR'
                        sel_mode   = 'A'
                        ).

    variant = VALUE #(
                         report = sy-repid
                         ).

    stable = VALUE #(
                        row = abap_true
                        col = abap_true
                        ).

  ENDMETHOD.
ENDCLASS.
