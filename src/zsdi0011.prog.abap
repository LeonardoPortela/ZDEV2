*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0011                                                *
* Descrição  : Carga de clientes                                       *
* Módulo     : SD                                                      *
* Transação  :                                                         *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Vagner R. dos Santo                    Data: 28/09/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
REPORT zsdi0011
       NO STANDARD PAGE HEADING LINE-SIZE 255.

*----------------------------------------------------------------------
* Tipos
TYPES: BEGIN OF y_excel,
       ktokd        TYPE rf02d-ktokd,
       kunnr        TYPE rf02d-kunnr,
       bukrs        TYPE rf02d-bukrs,
       vkorg        TYPE rf02d-vkorg,
       vtweg        TYPE rf02d-vtweg,
       spart        TYPE rf02d-spart,
       kalks        TYPE knvv-kalks,
       versg        TYPE knvv-versg,
       ktgrd        TYPE knvv-ktgrd,
       taxkd        TYPE knvi-taxkd,
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
           c_x            TYPE c VALUE 'X',
           c_xd01(4)      TYPE c VALUE 'XD01'.

*----------------------------------------------------------------------
* Tela de seleção
SELECTION-SCREEN: BEGIN OF BLOCK bl_001 WITH FRAME TITLE text-001.
PARAMETERS: p_file   LIKE rlgrap-filename OBLIGATORY.
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
      i_end_col               = 10
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

  DATA: opt TYPE ctu_params.

  LOOP AT ti_input INTO st_input.

    SHIFT st_input-bukrs RIGHT DELETING TRAILING space.
    OVERLAY st_input-bukrs WITH '0000'.

* Inicializar tabelas
    REFRESH: ti_bdcdata,
             ti_msg.
    CLEAR st_msg.

* Tela inicial
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'           '0100',
      ' '  'BDC_OKCODE'         '=ENTR',
      ' '  'RF02D-KTOKD'        st_input-ktokd,
      ' '  'RF02D-KUNNR'        st_input-kunnr,
      ' '  'RF02D-BUKRS'        st_input-bukrs,
      ' '  'RF02D-VKORG'        st_input-vkorg,
      ' '  'RF02D-VTWEG'        st_input-vtweg,
      ' '  'RF02D-SPART'        st_input-spart.

* Aba Vendas
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'           '0310',
   	 	' '  'BDC_OKCODE'	        'VW',
   	 	' '  'KNVV-KALKS'	        st_input-kalks,
   	 	' '  'KNVV-VERSG'	        st_input-versg.

* Aba Expedição
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'           '0315',
      ' '  'BDC_OKCODE'         'VW',
      ' '  'KNVV-ANTLF'         space.

* Tela Aba Doc. Faturamento
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'           '0320',
      ' '  'BDC_OKCODE'         'VW',
      ' '  'KNVV-KTGRD'         st_input-ktgrd.

* Tela Aba Doc. Faturamento
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'           '1350',
      ' '  'BDC_OKCODE'         'VW',
      ' '  'KNVI-TAXKD(01)'     st_input-taxkd.

* Tela Parceiros
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'           '0324',
      ' '  'BDC_OKCODE'         '=UPDA'.

* Inicializar campos utilizados no call transaction
    opt-dismode = 'N'.
    opt-updmode = 'S'.
    opt-defsize = 'X'.

* Executar transação XD01
* ---> S4 Migration - 29/06/2023 - JS
*    CALL TRANSACTION c_xd01
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
    PERFORM z_listar_mensagem.

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
FORM z_listar_mensagem .

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
    WRITE: /001 st_input-kunnr,
            012 vl_return.

  ENDLOOP.

ENDFORM.                    " Z_LISTAR_MENSAGEM
