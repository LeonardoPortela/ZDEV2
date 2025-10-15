*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0011                                                *
* Descrição  : Ajuste de domicílio fiscal, telefone e bairro de        *
*            : clientes e fornecedores.                                *
* Módulo     : SD                                                      *
* Transação  :                                                         *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Vagner R. dos Santo                    Data: 29/09/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*
REPORT zsdi0012
       NO STANDARD PAGE HEADING LINE-SIZE 255.

*----------------------------------------------------------------------
* Tabelas
TABLES: lfa1,   "Mestre de fornecedores (parte geral)
        kna1,   "Mestre de clientes (parte geral)
        j_1btreg_city.
* Tipos
TYPES: BEGIN OF y_for_cli,
       cli_for(10)  TYPE c,
       land1        TYPE lfa1-land1,
       regio        TYPE lfa1-regio,
       pstlz        TYPE lfa1-pstlz,
      END OF y_for_cli,

      BEGIN OF y_j1btreg_city,
         country       TYPE j_1btreg_city-country,
         region        TYPE j_1btreg_city-region,
         pstcd_from    TYPE j_1btreg_city-pstcd_from,
         pstcd_to      TYPE j_1btreg_city-pstcd_to,
         taxjurcode    TYPE j_1btreg_city-taxjurcode,
      END OF y_j1btreg_city,

      BEGIN OF y_dados_cor,
       cli_for(10)  TYPE c,
       addrnumber   TYPE adrc-addrnumber,
       city         TYPE adrc-city2,
       tel_number   TYPE adrc-tel_number,
      END OF y_dados_cor,

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
DATA: ti_lfa1             TYPE TABLE OF y_for_cli,
      ti_kna1             TYPE TABLE OF y_for_cli,
      ti_j1btreg_city     TYPE TABLE OF y_j1btreg_city,
      ti_dados_cor        TYPE TABLE OF y_dados_cor,
      ti_bdcdata          TYPE TABLE OF y_bdcdata,
      ti_msg              TYPE TABLE OF y_msg.

* Tabelas internas
DATA: st_bdcdata      TYPE y_bdcdata,
      st_msg          TYPE y_msg,
      st_lfa1         TYPE y_for_cli,
      st_kna1         TYPE y_for_cli,
      st_j1btreg_city TYPE y_j1btreg_city,
      st_dados_cor    TYPE y_dados_cor.

* Constantes
CONSTANTS: c_s            TYPE c VALUE 'S',
           c_x            TYPE c VALUE 'X',
           c_br(2)        TYPE c VALUE 'BR',
           c_xd02(4)      TYPE c VALUE 'XD02',   "Clientes
           c_xk02(4)      TYPE c VALUE 'XK02',   "Fornecedores
           c_centro(6)    TYPE c VALUE 'Centro'.

*----------------------------------------------------------------------
* Tela de seleção
SELECTION-SCREEN: BEGIN OF BLOCK bl_001 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr,
                s_kunnr FOR kna1-kunnr,
                s_domf  FOR j_1btreg_city-taxjurcode NO INTERVALS.
SELECTION-SCREEN: END OF BLOCK bl_001.

SELECTION-SCREEN: BEGIN OF BLOCK bl_002 WITH FRAME TITLE text-016.
SELECT-OPTIONS: st_lifnr FOR lfa1-lifnr,
                st_kunnr FOR kna1-kunnr.
SELECTION-SCREEN: END OF BLOCK bl_002.

*----------------------------------------------------------------------
* Validar tela
AT SELECTION-SCREEN.

  IF s_lifnr[] IS INITIAL AND
     s_kunnr[] IS INITIAL AND
     st_lifnr[] IS INITIAL AND
     st_kunnr[] IS INITIAL.
    MESSAGE e000(zles) WITH text-015.
  ELSEIF s_lifnr[] IS NOT INITIAL AND
         s_kunnr[] IS NOT INITIAL.
    MESSAGE e000(zles) WITH text-012 text-013.
  ENDIF.

*----------------------------------------------------------------------
* Seleção de dados.
START-OF-SELECTION.

* Tratar domicílio fiscal
* Processar fornecedores se o mesmo tiver sido informado
  IF s_lifnr-low IS NOT INITIAL.
    PERFORM z_obter_dom_fical_fornecedores.
  ENDIF.

  IF s_kunnr-low IS NOT INITIAL.
    PERFORM z_obter_dom_fical_clientes.
  ENDIF.

* Tratar telefone e bairro
* Processar fornecedores se o mesmo tiver sido informado
  IF st_lifnr-low IS NOT INITIAL.
    PERFORM z_obter_fornecedores.
    PERFORM z_tratar_dados.
  ENDIF.

  IF st_kunnr-low IS NOT INITIAL.
    PERFORM z_obter_clientes.
    PERFORM z_tratar_dados.
  ENDIF.

*----------------------------------------------------------------------
END-OF-SELECTION.
* Atualizar domicílio fiscal de fornecedores
  IF s_lifnr-low IS NOT INITIAL.
    PERFORM z_executar_xk02.      "Fornecedores
  ENDIF.

* Atualizar domicílio fiscal de clientes
  IF s_kunnr-low IS NOT INITIAL.
    PERFORM z_executar_xd02.      "Clientes
  ENDIF.

* Atualizar endereço e telefone de fornecedores
  IF st_lifnr-low IS NOT INITIAL.
    PERFORM z_atualizar_fornecedor.
  ENDIF.

* Atualizar endereço e telefone de fornecedores
  IF st_kunnr-low IS NOT INITIAL.
    PERFORM z_atualizar_clientes.
  ENDIF.

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
FORM z_listar_mensagem USING p_cli_for.

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
    WRITE: /001 p_cli_for,
            012 vl_return.

  ENDLOOP.

* Limpar tabela de mensagem.
  REFRESH ti_msg.

ENDFORM.                    " Z_LISTAR_MENSAGEM
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_DOM_FICAL_FORNECEDORES
*&---------------------------------------------------------------------*
* Selecionar fornecedor e domicílio fiscal
*----------------------------------------------------------------------*
FORM z_obter_dom_fical_fornecedores .

  REFRESH ti_lfa1.
  SELECT lifnr land1 regio pstlz INTO TABLE ti_lfa1
               FROM lfa1
        WHERE lifnr IN s_lifnr.

  IF ti_lfa1[] IS INITIAL.
    MESSAGE s000(zles) WITH text-014.
  ENDIF.
  CHECK ti_lfa1[] IS NOT INITIAL.

  REFRESH ti_j1btreg_city.
  SELECT country region pstcd_from pstcd_to taxjurcode
      INTO TABLE ti_j1btreg_city
      FROM j_1btreg_city
   FOR ALL ENTRIES IN ti_lfa1
      WHERE country     = ti_lfa1-land1
        AND region      = ti_lfa1-regio
        AND pstcd_from <= ti_lfa1-pstlz
        AND pstcd_to   >= ti_lfa1-pstlz
        AND taxjurcode IN s_domf.

ENDFORM.                    " Z_OBTER_DOM_FICAL_FORNECEDORES
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_DOM_FICAL_CLIENTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_obter_dom_fical_clientes.

  REFRESH ti_kna1.
  SELECT kunnr land1 regio pstlz INTO TABLE ti_kna1
               FROM kna1
        WHERE kunnr IN s_kunnr.

  IF ti_kna1[] IS INITIAL.
    MESSAGE s000(zles) WITH text-014.
  ENDIF.
  CHECK ti_kna1[] IS NOT INITIAL.

  REFRESH ti_j1btreg_city.
  SELECT country region pstcd_from pstcd_to taxjurcode
      INTO TABLE ti_j1btreg_city
      FROM j_1btreg_city
   FOR ALL ENTRIES IN ti_kna1
      WHERE country     = ti_kna1-land1
        AND region      = ti_kna1-regio
        AND pstcd_from <= ti_kna1-pstlz
        AND pstcd_to   >= ti_kna1-pstlz
        AND taxjurcode IN s_domf.

ENDFORM.                    " Z_OBTER_DOM_FICAL_CLIENTES

*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTAR_XK02
*&---------------------------------------------------------------------*
* Atualizar o domicílio fiscal do fornecedor
*----------------------------------------------------------------------*
FORM z_executar_xk02 .

  SORT ti_j1btreg_city BY country region pstcd_from pstcd_to.

  DATA: opt      TYPE ctu_params,
        vl_tabix TYPE sy-tabix.

  LOOP AT ti_lfa1 INTO st_lfa1.

    vl_tabix = 1.
    DO.
      READ TABLE ti_j1btreg_city INTO st_j1btreg_city INDEX vl_tabix.
      IF sy-subrc NE 0.
        EXIT.
      ELSEIF st_j1btreg_city-pstcd_from > st_lfa1-pstlz OR
             st_j1btreg_city-pstcd_to   < st_lfa1-pstlz.
        ADD 1 TO vl_tabix.
        CONTINUE.
      ENDIF.
      ADD 1 TO vl_tabix.

* Inicializar tabelas
      REFRESH: ti_bdcdata,
               ti_msg.
      CLEAR st_msg.

* Tela inicial
      PERFORM z_preenche_dbc USING:
        'X'  'SAPMF02K'           '0101',
        ' '  'BDC_OKCODE'         '/00',
        ' '  'RF02K-LIFNR'        st_lfa1-cli_for,
        ' '  'RF02K-D0120'        c_x.

* Aba Endereço
      PERFORM z_preenche_dbc USING:
        'X'  'SAPMF02K'              '0120',
        ' '  'LFA1-TXJCD'            st_j1btreg_city-taxjurcode,
        ' '  'BDC_OKCODE'            '=UPDA'.

* Inicializar campos utilizados no call transaction
      opt-dismode = 'N'.
      opt-updmode = 'S'.
      opt-defsize = 'X'.

* Executar transação XK02
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
      PERFORM z_listar_mensagem USING st_lfa1-cli_for.
* Sair do processamento "Do".
      EXIT.
    ENDDO.

  ENDLOOP.

ENDFORM.                    " Z_EXECUTAR_XK02
*&---------------------------------------------------------------------*
*&      Form  Z_EXECUTAR_XD02
*&---------------------------------------------------------------------*
* Modificar o domicílio fiscal do cliente.
*----------------------------------------------------------------------*
FORM z_executar_xd02 .

  DATA: opt      TYPE ctu_params,
        vl_tabix TYPE sy-tabix.

  LOOP AT ti_kna1 INTO st_kna1.

    vl_tabix = 1.
    DO.
      READ TABLE ti_j1btreg_city INTO st_j1btreg_city INDEX vl_tabix.
      IF sy-subrc NE 0.
        EXIT.
      ELSEIF st_j1btreg_city-pstcd_from > st_kna1-pstlz OR
             st_j1btreg_city-pstcd_to   < st_kna1-pstlz.
        ADD 1 TO vl_tabix.
        CONTINUE.
      ENDIF.
      ADD 1 TO vl_tabix.

* Inicializar tabelas
      REFRESH: ti_bdcdata,
               ti_msg.
      CLEAR st_msg.

* Tela inicial
      PERFORM z_preenche_dbc USING:
        'X'  'SAPMF02D'           '0101',
        ' '  'RF02D-KUNNR'        st_kna1-cli_for,
        ' '  'RF02D-D0120'        c_x,
        ' '  'BDC_OKCODE'         '=ENTR'.

* Aba Endereço
      PERFORM z_preenche_dbc USING:
        'X'  'SAPMF02D'           '0120',
        ' '  'KNA1-TXJCD'         st_j1btreg_city-taxjurcode,
        ' '  'BDC_OKCODE'         '=UPDA'.

* Inicializar campos utilizados no call transaction
      opt-dismode = 'N'.
      opt-updmode = 'S'.
      opt-defsize = 'X'.

* Executar transação XD02
* ---> S4 Migration - 29/06/2023 - JS
* Executar transação XD02
*    CALL TRANSACTION c_xd02
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
      PERFORM z_listar_mensagem USING st_kna1-cli_for.

* Sair do processamento "Do".
      EXIT.
    ENDDO.

  ENDLOOP.

ENDFORM.                    " Z_EXECUTAR_XD02
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_FORNECEDORES
*&---------------------------------------------------------------------*
* Obter os fornecedores que terão o telefone e o bairro corrigidos.
*----------------------------------------------------------------------*
FORM z_obter_fornecedores.

  REFRESH ti_dados_cor.
  SELECT l~lifnr a~addrnumber a~city2 a~tel_number
                 INTO TABLE ti_dados_cor
                 FROM lfa1 AS l
                 INNER JOIN adrc AS a ON a~addrnumber = l~adrnr
                WHERE lifnr IN st_lifnr.

ENDFORM.                    " Z_OBTER_FORNECEDORES
*&---------------------------------------------------------------------*
*&      Form  Z_TRATAR_DADOS
*&---------------------------------------------------------------------*
* Corrigir os campos.
*----------------------------------------------------------------------*
FORM z_tratar_dados .

  DATA: vl_telefone(10) TYPE c,
        vl_tabix        TYPE sy-tabix.

  LOOP AT ti_dados_cor INTO st_dados_cor.

    vl_tabix = sy-tabix.

* Tratar o telefone
    IF st_dados_cor-tel_number IS NOT INITIAL.
      REPLACE ALL OCCURRENCES OF REGEX '[^0-9]' IN st_dados_cor-tel_number WITH ''.
* Alinhar a direita e preencher o campo com zeros a esquerda
      IF STRLEN( st_dados_cor-tel_number ) < 10.
        vl_telefone = st_dados_cor-tel_number.
        SHIFT vl_telefone RIGHT DELETING TRAILING space.
        OVERLAY vl_telefone WITH '0000000000'.
        st_dados_cor-tel_number = vl_telefone.
      ELSEIF STRLEN( st_dados_cor-tel_number ) > 10.
        DO.
* Diminuir o tamanho do telefone até atingir 10 posições
          IF STRLEN( st_dados_cor-tel_number ) > 10.
            CLEAR st_dados_cor-tel_number+0(1).
            SHIFT st_dados_cor-tel_number LEFT DELETING LEADING space.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDIF.

* Tratar o bairro
    IF st_dados_cor-city IS INITIAL.
      st_dados_cor-city = c_centro.
    ENDIF.

    MODIFY ti_dados_cor FROM st_dados_cor INDEX vl_tabix
                        TRANSPORTING tel_number city.

  ENDLOOP.

ENDFORM.                    " Z_TRATAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZAR_FORNECEDOR
*&---------------------------------------------------------------------*
* Executar a transação XK02 para atualizar o fornecedor
*----------------------------------------------------------------------*
FORM z_atualizar_fornecedor .

  DATA: opt      TYPE ctu_params.

  LOOP AT ti_dados_cor INTO st_dados_cor.

* Inicializar tabelas
    REFRESH: ti_bdcdata,
             ti_msg.
    CLEAR st_msg.

* Tela inicial
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02K'           '0101',
      ' '  'RF02K-LIFNR'        st_dados_cor-cli_for,
      ' '  'RF02K-D0110'        c_x,
      ' '  'BDC_OKCODE'         '/00'.

* Aba Endereço
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02K'              '0110',
      ' '  'LFA1-ORT02'            st_dados_cor-city,
      ' '  'LFA1-TELF1'            st_dados_cor-tel_number,
      ' '  'BDC_OKCODE'            '=UPDA'.

* Janela domicílio fiscal
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMSSY0'              '0120',
      ' '  'BDC_OKCODE'            '=ENTR'.

* Inicializar campos utilizados no call transaction
    opt-dismode = 'N'.
    opt-updmode = 'S'.
    opt-defsize = 'X'.

* Executar transação XK02
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
    PERFORM z_listar_mensagem USING st_dados_cor-cli_for.

  ENDLOOP.

ENDFORM.                    " Z_ATUALIZAR_FORNECEDOR
*&---------------------------------------------------------------------*
*&      Form  Z_OBTER_CLIENTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_obter_clientes .

  REFRESH ti_dados_cor.
  SELECT k~kunnr a~addrnumber a~city2 a~tel_number
                 INTO TABLE ti_dados_cor
                 FROM kna1 AS k
                 INNER JOIN adrc AS a ON a~addrnumber = k~adrnr
                WHERE kunnr IN st_kunnr.

ENDFORM.                    " Z_OBTER_CLIENTES
*&---------------------------------------------------------------------*
*&      Form  Z_ATUALIZAR_CLIENTES
*&---------------------------------------------------------------------*
* Atualizar o telefone e a cidade dos clientes
*----------------------------------------------------------------------*
FORM z_atualizar_clientes .

  DATA: opt      TYPE ctu_params.

  LOOP AT ti_dados_cor INTO st_dados_cor.

* Inicializar tabelas
    REFRESH: ti_bdcdata,
             ti_msg.
    CLEAR st_msg.

* Tela inicial
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'           '7101',
      ' '  'RF02D-KUNNR'        st_dados_cor-cli_for,
      ' '  'BDC_OKCODE'         '=ENTR  '.

* Aba Endereço
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMF02D'              '7000',
      ' '  'ADDR1_DATA-CITY2'      st_dados_cor-city,
      ' '  'SZA1_D0100-TEL_NUMBER' st_dados_cor-tel_number,
      ' '  'BDC_OKCODE'            '=UPDA'.

* Janela de domicílio fiscal
    PERFORM z_preenche_dbc USING:
      'X'  'SAPMSSY0'           '0120',
      ' '  'BDC_OKCODE'         '=ENTR'.

* Inicializar campos utilizados no call transaction
    opt-dismode = 'N'.
    opt-updmode = 'S'.
    opt-defsize = 'X'.
    opt-nobinpt = 'X'.
    opt-nobiend = 'X'.

* ---> S4 Migration - 29/06/2023 - JS
* Executar transação XD02
*    CALL TRANSACTION c_xd02
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
    PERFORM z_listar_mensagem USING st_dados_cor-cli_for.

  ENDLOOP.

ENDFORM.                    " Z_ATUALIZAR_CLIENTES
