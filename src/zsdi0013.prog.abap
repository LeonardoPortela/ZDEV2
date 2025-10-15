*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0013                                                *
* Descrição  : Atualização de impostos de clientes                     *
* Módulo     : SD                                                      *
* Transação  :                                                         *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Vagner R. dos Santo                    Data: 30/09/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
REPORT zsdi0013
       NO STANDARD PAGE HEADING LINE-SIZE 255.

*----------------------------------------------------------------------
* Tipos
TYPES: BEGIN OF y_excel,
       lifnr        TYPE rf02k-lifnr,
       bukrs        TYPE rf02k-bukrs,
*       witht        TYPE lfbw-witht,
*       wt_withcd    TYPE lfbw-wt_withcd,
*       st_subjct    TYPE lfbw-wt_subjct,
      END OF y_excel,

      BEGIN OF y_bdcdata,
        program  TYPE bdcdata-program,
        dynpro   TYPE bdcdata-dynpro,
        dynbegin TYPE bdcdata-dynbegin,
        fnam     TYPE bdcdata-fnam,
        fval     TYPE bdcdata-fval,
      END OF y_bdcdata,

      BEGIN OF y_msg,
        tcode   TYPE bdcmsgcoll-tcode,
        dyname  TYPE bdcmsgcoll-dyname,
        dynumb  TYPE bdcmsgcoll-dynumb,
        msgtyp  TYPE bdcmsgcoll-msgtyp,
        msgspra TYPE bdcmsgcoll-msgspra,
        msgid   TYPE bdcmsgcoll-msgid,
        msgnr   TYPE bdcmsgcoll-msgnr,
        msgv1   TYPE bdcmsgcoll-msgv1,
        msgv2   TYPE bdcmsgcoll-msgv2,
        msgv3   TYPE bdcmsgcoll-msgv3,
        msgv4   TYPE bdcmsgcoll-msgv4,
        env     TYPE bdcmsgcoll-env,
        fldname TYPE bdcmsgcoll-fldname,
      END OF y_msg.

* Tabelas internas
DATA: ti_input     TYPE TABLE OF y_excel,
      ti_bdcdata   TYPE TABLE OF y_bdcdata,
      ti_msg       TYPE TABLE OF y_msg.

* Tabelas internas
DATA: st_input        TYPE y_excel,
      st_bdcdata      TYPE y_bdcdata,
      st_msg          TYPE y_msg.

* Constantes
CONSTANTS: c_asc(3)       TYPE c VALUE 'ASC',
           c_s            TYPE c VALUE 'S',
           c_n            TYPE c VALUE 'N',
           c_x            TYPE c VALUE 'X',
           c_xk02(4)      TYPE c VALUE 'XK02',
* Constantes para transportadores
           c_ic(2)        TYPE c VALUE 'IC',
           c_f0(2)        TYPE c VALUE 'F0',
           c_in(2)        TYPE c VALUE 'IN',
           c_n4(2)        TYPE c VALUE 'N4',
           c_ss(2)        TYPE c VALUE 'SS',
           c_s1(2)        TYPE c VALUE 'S1',

* Constantes para produtores
           c_fc(2)        TYPE c VALUE 'FC',
           c_fo(2)        TYPE c VALUE 'F0',
           c_fr(2)        TYPE c VALUE 'FR',
           c_r0(2)        TYPE c VALUE 'R0',
           c_ft(2)        TYPE c VALUE 'FT',
           c_f1(2)        TYPE c VALUE 'F1',
           c_se(2)        TYPE c VALUE 'SE',
           c_s0(2)        TYPE c VALUE 'S0'.

*----------------------------------------------------------------------
* Tela de seleção
SELECTION-SCREEN: BEGIN OF BLOCK bl_001 WITH FRAME TITLE text-001.
PARAMETERS: p_file   LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN: BEGIN OF BLOCK bl_002 WITH FRAME TITLE text-012.
PARAMETERS: p_pro   RADIOBUTTON GROUP gr,
            p_trans RADIOBUTTON GROUP gr.
SELECTION-SCREEN: END OF BLOCK bl_002.
SELECTION-SCREEN: END OF BLOCK bl_001.

*----------------------------------------------------------------------
* Match code para o nome do arquivo
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.

*----------------------------------------------------------------------
* Seleção de dados.
START-OF-SELECTION.

  PERFORM z_buscar_dados.

END-OF-SELECTION.
*
  CHECK ti_input[] IS NOT INITIAL.
  PERFORM z_montar_transacao.

*&---------------------------------------------------------------------*
*&      Form  Z_BUSCAR_DADOS
*&---------------------------------------------------------------------*
* Buscar a planilha excel na máquina do usuário.
*----------------------------------------------------------------------*
FORM z_buscar_dados .

  DATA: tl_excel TYPE TABLE OF alsmex_tabline,
        st_excel TYPE          alsmex_tabline.

  FIELD-SYMBOLS: <fs_data>.

  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 2
      i_end_col               = 2
      i_end_row               = 6000
    TABLES
      intern                  = tl_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.

  LOOP AT tl_excel INTO st_excel.

    AT NEW row.
      CLEAR st_input.
      APPEND st_input TO ti_input.
    ENDAT.

    ASSIGN COMPONENT st_excel-col OF STRUCTURE st_input TO <fs_data>.
    IF NOT <fs_data> IS ASSIGNED.
      CONTINUE.
    ENDIF.

    <fs_data> = st_excel-value.

    MODIFY ti_input FROM st_input INDEX st_excel-row.

    UNASSIGN <fs_data>.

  ENDLOOP.

* Exibir mensagem de erro, em caso do arquivo estar vazio.
  IF ti_input IS INITIAL.
    MESSAGE s000(zles) WITH text-002.
  ENDIF.

ENDFORM.                    " Z_BUSCAR_DADOOS
*&---------------------------------------------------------------------*
*&      Form  Z_MONTAR_TRANSACAO
*&---------------------------------------------------------------------*
* Montar as telas da transação XD01.
*----------------------------------------------------------------------*
FORM z_montar_transacao.

  DATA: opt               TYPE ctu_params,
        vl_tabix          TYPE sy-tabix,
        vl_indice(2)      TYPE n,
        vl_categ_irf(15)  TYPE c,
        vl_cod_irf(18)    TYPE c,
        vl_sujeicao(18)   TYPE c,
        vl_witht          TYPE lfbw-witht,
        vl_wt_withcd      TYPE lfbw-wt_withcd,
        vl_subjct         TYPE lfbw-wt_subjct,
        vl_fim_s_n        TYPE c.

  DATA: tl_input_aux TYPE TABLE OF y_excel,
        st_input_aux TYPE y_excel.

* Eliminar duplicidade de fornecedores na tabela auxiliar
  SORT ti_input BY lifnr.
  tl_input_aux[] = ti_input.
  DELETE ADJACENT DUPLICATES FROM tl_input_aux COMPARING lifnr.

  LOOP AT tl_input_aux INTO st_input_aux.

    SHIFT st_input_aux-bukrs RIGHT DELETING TRAILING space.
    OVERLAY st_input_aux-bukrs WITH '0000'.

* Inicializar tabelas
    REFRESH: ti_bdcdata,
             ti_msg.
    CLEAR st_msg.

* tela inicial
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02K'           '0101',
      ' '  'RF02K-LIFNR'        st_input_aux-lifnr,
      ' '  'RF02K-BUKRS'        st_input_aux-bukrs,
      ' '  'RF02K-D0610'        c_x,
      ' '  'BDC_OKCODE'         '=ENTR'.

* Tela Imposto Retido na Fonte
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02K'           '0610'.

* Obtém o ponteiro do registro
    READ TABLE ti_input INTO st_input WITH KEY lifnr = st_input_aux-lifnr.
    vl_tabix = sy-tabix.
    CLEAR vl_indice.
    vl_fim_s_n = c_n.

* Proessar todos os impostos do fornecedor
    DO.
* Sair do looping assim que o último imposto for montado.
      IF vl_fim_s_n = c_s.
        EXIT.
      ENDIF.

* Não será necessário criar controle para avançar tela, pois cada fornecedor
* terá no máximo 4 impostos.
*      READ TABLE ti_input INTO st_input INDEX vl_tabix.
*      IF sy-subrc NE 0 OR
*         st_input-lifnr NE st_input_aux-lifnr.
*        EXIT.
*      ENDIF.

*      ADD 1 TO vl_indice.
      vl_indice = sy-index.
      CONCATENATE 'LFBW-WITHT('
                  vl_indice
                  ')'
                  INTO vl_categ_irf.
      CONCATENATE 'LFBW-WT_WITHCD('
                  vl_indice
                  ')'
                  INTO vl_cod_irf.
      CONCATENATE 'LFBW-WT_SUBJCT('
                  vl_indice
                  ')'
                  INTO vl_sujeicao.

      IF p_trans = c_x.
* O arquivo tem como origem Transportador
*        CASE vl_tabix.
        CASE sy-index.
          WHEN 1.
            vl_witht     = c_ic.
            vl_wt_withcd = c_f0.
            vl_subjct    = c_x.
          WHEN 2.
            vl_witht     = c_in.
            vl_wt_withcd = c_n4.
            vl_subjct    = c_x.
          WHEN 3.
            vl_witht     = c_ss.
            vl_wt_withcd = c_s1.
            vl_subjct    = c_x.
            vl_fim_s_n   = c_s.
        ENDCASE.
      ELSE.
* O arquivo tem como origem Produtor
*        CASE vl_tabix.
        CASE sy-index.
          WHEN 1.
            vl_witht     = c_fc.
            vl_wt_withcd = c_f0.
            vl_subjct    = c_x.
*          WHEN 2.
*            vl_witht     = c_fr.
*            vl_wt_withcd = c_r0.
*            vl_subjct    = c_x.
          WHEN 2.
            vl_witht     = c_ft.
            vl_wt_withcd = c_f1.
            vl_subjct    = c_x.
            vl_fim_s_n   = c_s.
          WHEN 4.
*            vl_witht     = c_se.
*            vl_wt_withcd = c_s0.
*            vl_subjct    = c_x.
        ENDCASE.
      ENDIF.
      PERFORM z_preenche_dbc USING:
*        ' '  vl_categ_irf         st_input-witht,
*        ' '  vl_cod_irf           st_input-wt_withcd,
*        ' '  vl_sujeicao          st_input-st_subjct.
        ' '  vl_categ_irf         vl_witht,
        ' '  vl_cod_irf           vl_wt_withcd,
        ' '  vl_sujeicao          vl_subjct.

*      ADD 1 TO vl_tabix.

    ENDDO.

    PERFORM z_preenche_dbc USING:
      ' '  'BDC_OKCODE'         '=UPDA'.

* Inicializar campos utilizados no call transaction
    opt-dismode = 'N'.
    opt-updmode = 'S'.
    opt-defsize = 'X'.

* Executar transação XD01
* ---> S4 Migration - 29/06/2023 - JS
*    CALL TRANSACTION c_xk02
*       USING ti_bdcdata
*       OPTIONS FROM opt
*       MESSAGES INTO ti_msg.

      DATA: lt_bdc    TYPE bdcdata_tab,
            lt_bdcmsg TYPE tab_bdcmsgcoll,
            wa_lfa1   type lfa1.

      DATA: lo_migbp TYPE REF TO /mignow/cl_migbp.

      lt_bdc = CONV #( ti_bdcdata[] ).

      CREATE OBJECT lo_migbp
        EXPORTING
          im_test    = abap_false
          im_tcode   = 'BP'
          it_bdcdata = lt_bdc.

      CALL METHOD lo_migbp->mt_bp_process_old_shdb(
        CHANGING
          ct_bdcmsg = lt_bdcmsg ).

      CALL METHOD lo_migbp->mt_set_data_directly( is_lfa1 = wa_lfa1 ).

      CALL METHOD lo_migbp->mt_bp_process_data( CHANGING ct_bdcmsg = lt_bdcmsg ).

      ti_msg = CONV #( lt_bdcmsg[] ).
* <--- S4 Migration - 29/06/2023 - JS

* Apagar mensagem de sucesso.
    DELETE ti_msg WHERE msgtyp = c_s.
* Direcionar para rotina de impressão se houver dados.
    CHECK ti_msg[] IS NOT INITIAL.
    PERFORM z_listar_mensagem USING st_input_aux-lifnr.

  ENDLOOP.

ENDFORM.                    " Z_MONTAR_TRANSACAO
*&---------------------------------------------------------------------*
*&      Form  Z_PREENCHE_DBC
*&---------------------------------------------------------------------*
* Rotina de montagem das telas
*----------------------------------------------------------------------*
*      -->P_0220   text
*      -->P_0221   text
*      -->P_0222   text
*----------------------------------------------------------------------*
FORM z_preenche_dbc  USING  p_dynbegin TYPE any
                            p_name     TYPE any
                            p_value    TYPE any.


  IF p_dynbegin = c_x.

    MOVE: p_name     TO st_bdcdata-program,
          p_value    TO st_bdcdata-dynpro,
          p_dynbegin TO st_bdcdata-dynbegin.

  ELSE.

    MOVE: p_name     TO st_bdcdata-fnam,
          p_value    TO st_bdcdata-fval.

  ENDIF.

  APPEND st_bdcdata TO ti_bdcdata.
  CLEAR: st_bdcdata.

ENDFORM.                    " Z_PREENCHE_DBC

*&---------------------------------------------------------------------*
* Impressão do cabeçalho do relatório.
TOP-OF-PAGE.

  WRITE: 001 sy-repid,
         093 text-001, sy-datum,   "Hora
        /040 text-011,             "Lista de erros
         093 text-003, sy-uzeit,   "hora
        /093 text-004, sy-pagno USING EDIT MASK 'RR___'.   "Página

  SKIP 2.

  WRITE: 001 text-005,      "Código do cliente.
         012 text-009,      "Erro
        /001 '----------',
         012 '------------------------------------------------------',
         066 '------------------------------------------------------'.
*&---------------------------------------------------------------------*
*&      Form  Z_LISTAR_MENSAGEM
*&---------------------------------------------------------------------*
* Gerar relatório de inconsistências
*----------------------------------------------------------------------*
FORM z_listar_mensagem USING p_lifnr.

  DATA vl_return TYPE string.

* Obter o(s) texto(s) da(s) mensagem(s) de erro.
  LOOP AT ti_msg INTO st_msg.

    CALL FUNCTION 'MESSAGE_PREPARE'
      EXPORTING
        msg_id                 = st_msg-msgid
        msg_no                 = st_msg-msgnr
        msg_var1               = st_msg-msgv1(50)
        msg_var2               = st_msg-msgv2(50)
        msg_var3               = st_msg-msgv3(50)
        msg_var4               = st_msg-msgv4(50)
      IMPORTING
        msg_text               = vl_return
      EXCEPTIONS
        function_not_completed = 1
        message_not_found      = 2
        OTHERS                 = 3.

    CHECK sy-subrc EQ 0.
    WRITE: /001 p_lifnr,
            012 vl_return.

  ENDLOOP.

ENDFORM.                    " Z_LISTAR_MENSAGEM
