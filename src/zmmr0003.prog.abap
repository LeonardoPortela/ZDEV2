*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 26/07/2010                                              &*
*& Descrição: ALGODOEIRA – Romaneio de Saida                          &*
*& Transação: ZMM0027                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   20.07.2010                            &*
*&--------------------------------------------------------------------&*

REPORT  zmmr0003.
*----------------------------------------------------------------------*
* Tabelas
*----------------------------------------------------------------------*
TABLES: j_1bbranch, j_1bnflin, zsdt0001, j_1bnfe_active, zmmt0126.
*----------------------------------------------------------------------*
* TIPOS PARA ALV
*----------------------------------------------------------------------*
TYPE-POOLS: slis.

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_zsdt0001,
         nr_romaneio TYPE zsdt0001-nr_romaneio,
         vbeln       TYPE zsdt0001-vbeln,
         branch      TYPE zsdt0001-branch,
         doc_rem     TYPE zsdt0001-doc_rem,
         id_cli_dest TYPE zsdt0001-id_cli_dest,
       END OF ty_zsdt0001,

       BEGIN OF ty_vbrp,
         vbeln  TYPE vbrp-vbeln,
         vgbel  TYPE vbrp-vgbel,
         matnr  TYPE vbrp-matnr,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_vbrp,

       BEGIN OF ty_lin,
         docnum TYPE j_1bnflin-docnum,
         refkey TYPE j_1bnflin-refkey,
       END OF ty_lin,

       BEGIN OF ty_bnfe,
         docnum TYPE j_1bnfe_active-docnum,
         nfnum9 TYPE j_1bnfe_active-nfnum9,
         bukrs  TYPE j_1bnfe_active-bukrs,
         branch TYPE j_1bnfe_active-branch,
       END OF ty_bnfe,

       BEGIN OF ty_zmmt0008,
         werks              TYPE zmmt0008-werks,
         lgort              TYPE zmmt0008-lgort,
         nr_romaneio        TYPE zmmt0008-nr_romaneio,
         charg              TYPE zmmt0008-charg,
         menge              TYPE zmmt0008-menge,
         werks_orig         TYPE zmmt0008-werks_orig,
         lgortr             TYPE lgort_d,  "*-S4H-US 123905-25.09.2023-JT
         safra              TYPE charg_d,  "*-S4H-US 123905-25.09.2023-JT
         mblnr              TYPE mblnr,    "*-S4H-US 123905-25.09.2023-JT
         mjahr              TYPE mjahr,    "*-S4H-US 123905-25.09.2023-JT
         dados_contingencia TYPE char01,   "*-S4H-US 123905-25.09.2023-JT
         cd_sai             TYPE zmmt0008-cd_sai,
         tipo_fardo         TYPE zmmt0008-tipo_fardo,
       END OF ty_zmmt0008,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         normt TYPE mara-normt,
       END OF ty_mara,

       BEGIN OF ty_total,
         total TYPE sy-tabix,
         tipo  TYPE mara-normt,
         peso  TYPE zmmt0008-menge,
       END OF ty_total,

       BEGIN OF ty_saida,
         mark               TYPE c,
         werks              TYPE zmmt0008-werks,
         bloco              TYPE zmmt0008-lgort,
         fardo              TYPE zmmt0008-charg,
         nr_romaneio        TYPE zmmt0008-nr_romaneio,
         nfnum              TYPE zmmt0008-nfnum,
         vbeln              TYPE zmmt0008-vbeln,
         vbeln_vf           TYPE zmmt0008-vbeln_vf,
         placa_cav          TYPE zmmt0008-placa_cav,
         matnr              TYPE vbrp-matnr,
         menge              TYPE zmmt0008-menge,
         werks_orig         TYPE zmmt0008-werks_orig,
         lgortr             TYPE lgort_d,  "*-S4H-US 123905-25.09.2023-JT
         safra              TYPE charg_d,  "*-S4H-US 123905-25.09.2023-JT
         mblnr              TYPE mblnr,    "*-S4H-US 123905-25.09.2023-JT
         mjahr              TYPE mjahr,    "*-S4H-US 123905-25.09.2023-JT
         dados_contingencia TYPE char01,   "*-S4H-US 123905-25.09.2023-JT
         cd_sai             TYPE zmmt0008-cd_sai,
         tipo_fardo         TYPE zmmt0008-tipo_fardo,
*         charg       TYPE zmmt0008-charg,
*         lgort       TYPE zmmt0008-lgort,

       END OF ty_saida.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.


TYPES: BEGIN OF ty_excel,
         empresa    TYPE zmms005-empresa,
         safra      TYPE zmms005-safra,
         remetente  TYPE zmms005-remetente,
         terminal   TYPE zmms005-destinatario,
         cliente    TYPE zmms005-cliente,
         romaneio   TYPE zmms005-romaneio,
         nfnum      TYPE zmms005-nfnum,
         motorista  TYPE zmms005-motorista,
         veiculo    TYPE zmms005-veiculo,
         bloco      TYPE zmms005-bloco,
         qtd_fardos TYPE zmms006-total,
         peso_total TYPE zmms004-peso,
         instrucao  TYPE zsdt0066-instrucao,
         stcd1      TYPE j_1bbranch-stcd1,
       END OF ty_excel.


TYPES: BEGIN OF ty_fardos_exc,
         fardos  TYPE char21,
         peso    TYPE zmms004-peso,
         tipo    TYPE zmms004-tipo,
         tamanho TYPE zmmt0008-tipo_fardo,
       END OF ty_fardos_exc.

TYPES: BEGIN OF ty_zsdt0045,
         zseq_inst     TYPE zsdt0045-zseq_inst,
         objek         TYPE zsdt0045-objek,
         objecttable   TYPE zsdt0045-objecttable,
         booking       TYPE zsdt0045-booking,
         instrucao     TYPE zsdt0045-instrucao,
         tamanho_fardo TYPE zsdt0045-tamanho_fardo,
       END OF ty_zsdt0045.

DATA: it_zmail TYPE STANDARD TABLE OF zmail,
      wa_zmail TYPE zmail.


*&--------------------------------------------------------------------&*
*& Declaração de Constantes                                           &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_s TYPE c VALUE 'S',
           c_0 TYPE c VALUE '0',
           c_1 TYPE c VALUE '1',
           c_x TYPE c VALUE 'X'.

*&--------------------------------------------------------------------&*
*& Declaração de Tabelas/Workareas                                    &*
*&--------------------------------------------------------------------&*
DATA: tg_zsdt0001     TYPE TABLE OF ty_zsdt0001,
      wg_zsdt0001     TYPE ty_zsdt0001,
      tg_vbrp         TYPE TABLE OF ty_vbrp,
      wg_vbrp         TYPE ty_vbrp,
      tg_lin          TYPE TABLE OF ty_lin,
      wg_lin          TYPE ty_lin,
      tg_bnfe         TYPE TABLE OF ty_bnfe,
      wg_bnfe         TYPE ty_bnfe,
      tg_zmmt0008     TYPE TABLE OF ty_zmmt0008,
      tg_zmmt0008_aux TYPE TABLE OF ty_zmmt0008,
      wg_zmmt0008     TYPE ty_zmmt0008,
      tg_kna1         TYPE TABLE OF ty_kna1,
      wg_kna1         TYPE ty_kna1,
      tg_saida        TYPE TABLE OF ty_saida,
      wg_saida        TYPE ty_saida,
      wg_header       TYPE zmms005,
      tg_fardos       TYPE TABLE OF zmms004 WITH HEADER LINE,
      tg_mara         TYPE TABLE OF ty_mara,
      wg_mara         TYPE ty_mara,
      tg_excel        TYPE TABLE OF ty_excel,
      wg_excel        TYPE ty_excel,
      tg_fardos_exc   TYPE TABLE OF ty_fardos_exc,
      wg_fardos_exc   TYPE ty_fardos_exc,
      wa_zsdt0045     TYPE ty_zsdt0045,
      v_name(30),
      v_input,
      v_required,
      v_request,
      v_value,
      v_txtbtn        TYPE c LENGTH 50,
      v_init          TYPE c,
      l_erro          TYPE c,
      l_mesg1         TYPE char200,
      l_mesg2         TYPE char200,
      l_mesg3         TYPE char200.

DATA: send_request  TYPE REF TO cl_bcs,
      document      TYPE REF TO cl_document_bcs,
      recipient     TYPE REF TO if_recipient_bcs,
      bcs_exception TYPE REF TO cx_bcs.

DATA: main_text      TYPE bcsy_text,
      binary_content TYPE solix_tab,
      size           TYPE so_obj_len,
      sent_to_all    TYPE os_boolean.


*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      l_roman_ant  LIKE zmmt0008-nr_romaneio,
      wg_layout    TYPE slis_layout_alv,
      t_top        TYPE slis_t_listheader.

*&--------------------------------------------------------------------&*
*& Tela de Seleção                                                    &*
*&--------------------------------------------------------------------&*
DATA teste TYPE char8.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
  PARAMETERS: p_um   RADIOBUTTON GROUP a1 USER-COMMAND asnum,
              p_dois RADIOBUTTON GROUP a1,
              p_tres RADIOBUTTON GROUP a1.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(50) TEXT-005 FOR FIELD p_quatro .
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: p_quatro RADIOBUTTON GROUP a1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN: END OF BLOCK b2.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_roman     TYPE zmmt0008-nr_romaneio MODIF ID bbb,
              p_safra     TYPE zsdt0001-nr_safra MODIF ID bbb OBLIGATORY DEFAULT sy-datum+0(4),
              p_werks     TYPE zmmt0008-werks MODIF ID bbb,
              p_bloco     TYPE zmmt0008-lgort MODIF ID bbb,
              p_motor(50) MODIF ID aaa,
              p_placa     TYPE zmmt0008-placa_cav  MODIF ID aaa,
              p_kunnr     TYPE kna1-kunnr MODIF ID aaa.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(20) TEXT-008 FOR FIELD p_local  MODIF ID ddd.
    SELECTION-SCREEN POSITION 33.
    SELECT-OPTIONS: p_local  FOR zmmt0126-kunnr NO INTERVALS NO-EXTENSION  MODIF ID ddd.
    SELECTION-SCREEN COMMENT 50(40) v_descr  MODIF ID ddd. "TEXT-008.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN: END OF BLOCK b3.

*-CS2022000332-#78518 -30.04.2022-JT-inicio
AT SELECTION-SCREEN.
  PERFORM f_carrega_dados_selecao.
*-CS2022000332-#78518 -30.04.2022-JT-fim

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_kunnr.

INITIALIZATION.

  v_name = 'AAA'.
  v_input = '1'.

  IF p_um IS INITIAL     AND
     p_dois IS INITIAL   AND
     p_tres IS INITIAL   AND
     p_quatro IS INITIAL.

    IMPORT v_init FROM MEMORY ID 'SEL'.
    FREE MEMORY ID 'SEL'.

    IF v_init = 4.
      p_quatro = 'X'.
    ELSE.
      v_name = 'DDD'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.
    ENDIF.

  ENDIF.

  IF p_quatro IS NOT INITIAL.

    PERFORM zf_valida_set USING v_value.

    IF v_value IS NOT INITIAL.

      v_name = 'DDD'.
      v_input = '1'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.
    ENDIF.

  ENDIF.

AT SELECTION-SCREEN OUTPUT.


  CASE 'X'.
    WHEN p_um.

      v_name = 'AAA'.
      v_input = '1'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

      v_name = 'DDD'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

    WHEN p_dois OR p_tres.

*      V_NAME = 'P_MOTOR'.
      v_name = 'AAA'.
      v_input = '0'.
      CLEAR: p_placa, p_motor.
      PERFORM f_modificar_tela USING v_input
                                v_required
                                v_request.

      v_name = 'DDD'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

    WHEN  p_quatro.

      v_name = 'AAA'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

      v_name = 'BBB'.
      v_input = '0'.
      PERFORM f_modificar_tela USING v_input
                               v_required
                               v_request.

      PERFORM zf_valida_set USING v_value.

      IF v_value IS NOT INITIAL.

        v_name = 'DDD'.
        v_input = '1'.
        PERFORM f_modificar_tela USING v_input
                                 v_required
                                 v_request.

      ELSE.

        v_name = 'DDD'.
        v_input = '0'.
        PERFORM f_modificar_tela USING v_input
                                 v_required
                                 v_request.

      ENDIF.

    WHEN OTHERS.
  ENDCASE.

  IF p_local IS NOT INITIAL.

    CLEAR v_descr.
    SELECT SINGLE name1 FROM kna1
      INTO v_descr
      WHERE kunnr IN p_local.

  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_MODIFICAR_TELA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_modificar_tela USING input
                            required
                            request  .

  LOOP AT SCREEN.

*    IF SCREEN-NAME = V_NAME.
    IF screen-group1 = v_name.
      screen-active   = input.                              "'0'.
      screen-required = required.                           "'0'.
      screen-request  = request.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_MODIFICAR_TELA
*&--------------------------------------------------------------------&*
*& Start-Of-Selection                                                 &*
*&--------------------------------------------------------------------&*
START-OF-SELECTION.

  IF p_quatro IS INITIAL.


    IF p_roman IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do campo Número do Romaneio'.
      STOP.
    ENDIF.

    IF p_werks IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do campo Centro'.
      STOP.
    ENDIF.

    IF p_bloco IS INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do campo Depósito'.
      STOP.
    ENDIF.

    IF p_um EQ c_x.
      IF p_motor IS INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do campo Motorista'.
        STOP.
      ENDIF.

      IF p_placa IS INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do campo Placa do Veículo'.
        STOP.
      ENDIF.


      IF p_kunnr IS INITIAL.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'É obrigatorio o preenchimento do campo Cliente'.
        STOP.
      ENDIF.

      PERFORM seleciona_dados.
      PERFORM organiza_dados.
      PERFORM iniciar_variaves.
      PERFORM imprime_dados.
    ELSEIF p_dois EQ c_x.
      PERFORM seleciona_dados.
      PERFORM imprime_smartforms.
    ELSE.
      PERFORM seleciona_dados.
    ENDIF.

  ELSE.

    PERFORM zf_cadastro_email.
*      PERFORM ZF_CALL_TRANSACTION_EMAIL.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_DADOS_SELECAO.
*&---------------------------------------------------------------------*
FORM f_carrega_dados_selecao.

  CHECK sy-ucomm <> 'ONLI'.

  IF p_placa IS INITIAL OR ( p_roman <> l_roman_ant ).
    SELECT vbeln, id_cli_dest, placa_cav, motorista
      INTO TABLE @DATA(t_0001)
      FROM zsdt0001
     WHERE tp_movimento EQ 'S'                              "IR150285
       AND nr_romaneio = @p_roman
       AND nr_safra    = @p_safra
       AND branch      = @p_werks.

    IF sy-subrc = 0.
      READ TABLE t_0001 INTO DATA(w_0001) INDEX 1.
      IF sy-subrc = 0.
        p_placa = w_0001-placa_cav.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_motor IS INITIAL OR ( p_roman <> l_roman_ant ).
    SELECT vbeln id_cli_dest placa_cav motorista
      INTO TABLE t_0001
      FROM zsdt0001
     WHERE tp_movimento EQ 'S'                              "IR150285
       AND nr_romaneio = p_roman
       AND nr_safra    = p_safra
       AND branch      = p_werks.

    IF sy-subrc = 0.
      CLEAR w_0001.
      READ TABLE t_0001 INTO w_0001 INDEX 1.
      SELECT SINGLE name1
        INTO @DATA(l_name1)
        FROM lfa1
       WHERE lifnr = @w_0001-motorista.
      IF sy-subrc = 0.
        p_motor = l_name1.
      ENDIF.
    ENDIF.
  ENDIF.

  IF p_kunnr IS INITIAL OR ( p_roman <> l_roman_ant ).
    SELECT vbeln id_cli_dest placa_cav motorista
      INTO TABLE t_0001
      FROM zsdt0001
     WHERE tp_movimento EQ 'S'                              "IR150285
       AND nr_romaneio = p_roman
       AND nr_safra    = p_safra
       AND branch      = p_werks.

    IF sy-subrc = 0.
      LOOP AT t_0001 INTO w_0001.
        SELECT SINGLE auart
          INTO @DATA(l_auart)
          FROM vbak
         WHERE vbeln = @w_0001-vbeln.
        IF sy-subrc = 0 AND l_auart <> 'ZRFL'.
          p_kunnr = w_0001-id_cli_dest.
          EXIT.
        ELSE.
          CLEAR p_kunnr.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  l_roman_ant = p_roman.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .
  DATA: tl_0008        TYPE TABLE OF zmmt0008 WITH HEADER LINE,
        wl_cont        TYPE sy-tabix,
        msg(150),
        resposta,
        wl_cont_aux(6).

  REFRESH: tg_fardos.
  CLEAR: wg_header.

  IF p_um EQ c_x.
    SELECT nr_romaneio vbeln branch doc_rem id_cli_dest
      FROM zsdt0001
      INTO TABLE tg_zsdt0001
       WHERE tp_movimento EQ c_s
         AND nr_romaneio  EQ p_roman
         AND branch       EQ p_werks
         AND nr_safra     EQ p_safra.

    IF sy-subrc IS INITIAL.
      SELECT vbeln vgbel matnr
        FROM vbrp
        INTO TABLE tg_vbrp
        FOR ALL ENTRIES IN tg_zsdt0001
         WHERE vgbel EQ tg_zsdt0001-doc_rem.

      IF sy-subrc IS INITIAL.
        LOOP AT tg_vbrp INTO wg_vbrp.
*        CONCATENATE '00' WG_VBRP-VBELN INTO WG_VBRP-REFKEY.
          wg_vbrp-refkey = wg_vbrp-vbeln.
          MODIFY tg_vbrp FROM wg_vbrp.

        ENDLOOP.

        SELECT docnum refkey
          FROM j_1bnflin
          INTO TABLE tg_lin
           FOR ALL ENTRIES IN tg_vbrp
           WHERE refkey EQ tg_vbrp-refkey.

        IF sy-subrc IS INITIAL.
*** BUG 74610 INICIO BG
          SELECT docnum nfnum9 bukrs branch
            FROM j_1bnfe_active
            INTO TABLE tg_bnfe
             FOR ALL ENTRIES IN tg_lin
             WHERE docnum EQ tg_lin-docnum
             AND nfnum9 <> ' '.
*              AND  docsta = '1'
*              AND  scssta <> '2'
*              AND  cancel = '  '.
*** PBI - 64946 - Inicio - CSB
          IF tg_bnfe IS INITIAL.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Para Gerar o lote de Venda a NF-e do Romaneio informado deve estar Determinada'.
            STOP.
          ENDIF.
*** PBI - 64946 - Inicio - CSB

*** BUG 74610 - FIM - BG
        ENDIF.
      ENDIF.

      SELECT werks lgort nr_romaneio charg menge werks_orig
             lgortr safra mblnr mjahr dados_contingencia cd_sai tipo_fardo  "*-S4H-US 123905-25.09.2023-JT
        FROM zmmt0008
        INTO TABLE tg_zmmt0008
         WHERE werks EQ p_werks
           AND lgort EQ p_bloco
           AND safra EQ p_safra
           AND ( nr_romaneio EQ space OR
                 nr_romaneio EQ c_0  ).

*** PBI - 64946 - Inicio - CSB
      IF tg_zmmt0008 IS INITIAL.

        DATA: lva_count TYPE i.

        SELECT werks lgort nr_romaneio charg menge werks_orig
               lgortr safra mblnr mjahr dados_contingencia cd_sai tipo_fardo  "*-S4H-US 123905-25.09.2023-JT
          FROM zmmt0008
          INTO TABLE tg_zmmt0008_aux
            WHERE werks      EQ p_werks
             AND lgort       EQ p_bloco
             AND safra       EQ p_safra
             AND nr_romaneio EQ p_roman
             AND placa_cav   EQ p_placa
             AND kunnr       EQ p_kunnr
             AND status      EQ '3'.

        DESCRIBE TABLE tg_zmmt0008_aux LINES lva_count.
        IF lva_count > 0.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Romaneio está em processo de cancelamento'
                                                 'e estorno no sistema Rondonline,'
                                                 'por favor aguardar!'.
          STOP.
        ENDIF.
      ENDIF.
*** PBI - 64946 - Fim - CSB

      SELECT kunnr name1
        FROM kna1
        INTO TABLE tg_kna1
         FOR ALL ENTRIES IN tg_zsdt0001
         WHERE kunnr EQ tg_zsdt0001-id_cli_dest.

    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Romaneio informado não foi encontrado'.
      STOP.
    ENDIF.
  ELSEIF p_dois EQ c_x.
*    wg_header-motorista = p_motor.
    SELECT a~mandt a~werks a~lgort a~charg a~nr_romaneio a~nfnum
a~vbeln a~vbeln_vf a~placa_cav a~menge a~motorista a~kunnr a~integ_rondonline
a~dt_inicial_integ a~status a~id_hist_rondon a~werks_orig a~cd_sai a~tipo_fardo
      FROM zmmt0008 AS a
*--> CS1052840 / IR122792
      INNER JOIN vbap AS b ON a~vbeln = b~vbeln
*--> CS1052840 / IR122792
       INTO CORRESPONDING FIELDS OF TABLE tl_0008
       WHERE a~werks       EQ p_werks
         AND a~lgort       EQ p_bloco
         AND a~safra       EQ p_safra
         AND a~nr_romaneio EQ p_roman
         AND b~charg  EQ p_safra.

    IF sy-subrc NE 0.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados não encontrados.'.
      STOP.
    ENDIF.
    IF sy-subrc IS INITIAL.
      READ TABLE tl_0008 INDEX 1.

      IF tl_0008-status = '3'.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'O Romaneio está em processo de estorno'
                                          'no sistema Rondonline,'
                                          'impressão já não é mais possível!'.
        STOP.
      ENDIF.

      wg_header-veiculo   = tl_0008-placa_cav.
      wg_header-romaneio  = tl_0008-nr_romaneio.
      wg_header-motorista = tl_0008-motorista.
      wg_header-bloco     = tl_0008-lgort.
      SELECT SINGLE name1
        FROM kna1
        INTO wg_header-cliente
         WHERE kunnr EQ tl_0008-kunnr.

      SELECT SINGLE *
        FROM zsdt0001
*        INTO TABLE tg_zsdt0001
         WHERE vbeln EQ tl_0008-vbeln
           AND tp_movimento EQ c_s
           AND nr_romaneio  EQ p_roman
           AND branch       EQ p_werks
           AND vbeln        EQ tl_0008-vbeln.
      IF sy-subrc IS INITIAL.
        wg_header-safra = zsdt0001-nr_safra.


**    Destinatario
        SELECT SINGLE name1
          FROM kna1
          INTO wg_header-destinatario
           WHERE kunnr EQ zsdt0001-id_cli_dest.


        SELECT SINGLE *
          FROM j_1bnflin
           WHERE refkey EQ tl_0008-vbeln_vf.
        IF sy-subrc IS INITIAL.
          SELECT SINGLE *
            FROM j_1bnfe_active
             WHERE docnum EQ j_1bnflin-docnum.

          IF sy-subrc IS INITIAL.
**          Nota Fiscal
            wg_header-nfnum = j_1bnfe_active-nfnum9.
            SELECT SINGLE *
              FROM j_1bbranch
               WHERE bukrs  EQ j_1bnfe_active-bukrs
                 AND branch EQ j_1bnfe_active-branch.
** Remetente
            wg_header-remetente = j_1bbranch-name.
            SELECT SINGLE butxt
              FROM t001
              INTO wg_header-empresa
               WHERE bukrs EQ j_1bnfe_active-bukrs.


          ENDIF.
        ENDIF.

      ENDIF.

      SELECT vbeln vgbel matnr
        FROM vbrp
        INTO TABLE tg_vbrp
        FOR ALL ENTRIES IN tl_0008
         WHERE vbeln EQ tl_0008-vbeln_vf.
      IF sy-subrc IS INITIAL.
        SELECT matnr normt
          FROM mara
          INTO TABLE tg_mara
          FOR ALL ENTRIES IN tg_vbrp
           WHERE matnr EQ tg_vbrp-matnr.
      ENDIF.
      "DATA: fardo TYPE zppt0002-cd_sai.
      LOOP AT tl_0008.
        LOOP AT tg_mara INTO wg_mara.
          CONDENSE tl_0008-charg NO-GAPS.

          IF tl_0008-cd_sai IS INITIAL.
            IF tl_0008-charg+02(1) = '/'. "Projeto Reestruturação Algodao 2024
              SELECT SINGLE cd_sai
                FROM zppt0002 INTO tg_fardos-fardo
               WHERE acharg = tl_0008-charg
                 AND werks  = tl_0008-werks.
            ENDIF.
          ELSE.
            tg_fardos-fardo = tl_0008-cd_sai. "Projeto Reestruturação Algodao 2024
          ENDIF.

          tg_fardos-tipo  = wg_mara-normt(5).
          tg_fardos-peso  = tl_0008-menge.

          APPEND tg_fardos.
        ENDLOOP.
        CLEAR: tg_fardos.

      ENDLOOP.

    ENDIF.
  ELSE.
    DATA:  w_mensagem(80).
    TYPES: ty_selection TYPE STANDARD TABLE OF rsparams.

    DATA:
*     Define internal table and work area for select-options
      lwa_selection TYPE rsparams,
      lit_selection TYPE ty_selection.


*   SELECT  A~MANDT A~WERKS A~LGORT A~CHARG A~NR_ROMANEIO A~NFNUM
*           A~VBELN A~VBELN_VF A~PLACA_CAV A~MENGE A~MOTORISTA A~KUNNR A~INTEG_RONDONLINE
*           A~DT_INICIAL_INTEG A~STATUS A~ID_HIST_RONDON A~WERKS_ORIG

    SELECT a~mandt a~werks a~lgort a~charg a~nr_romaneio a~nfnum
           a~vbeln a~vbeln_vf a~placa_cav a~menge a~motorista a~kunnr a~integ_rondonline
           a~dt_inicial_integ a~status a~id_hist_rondon a~werks_orig a~cd_sai a~tipo_fardo a~safra
      FROM zmmt0008 AS a
*--> CS1050433 / IR121974
      INNER JOIN vbap AS b ON a~vbeln = b~vbeln
*--> CS1050433 / IR121974
      INTO CORRESPONDING FIELDS OF TABLE tl_0008
       WHERE a~werks       EQ p_werks
         AND a~lgort       EQ p_bloco
         AND a~safra       EQ p_safra
         AND a~nr_romaneio EQ p_roman
*--> CS1050433 / IR121974
         AND b~charg  EQ p_safra.


    IF sy-subrc NE 0.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados não encontrados.'.
      STOP.
    ENDIF.
*<-- CS1050433 / IR121974
*    DESCRIBE TABLE tl_0008 LINES wl_cont.
*    IF wl_cont GT 0.
*      wl_cont_aux = wl_cont.
*      CONCATENATE 'Serão cancelados' wl_cont_aux 'fardos de venda.' INTO msg SEPARATED BY space.
*
*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING
*          titlebar              = text-i07
*          text_question         = msg
*          text_button_1         = 'Ok'
*          icon_button_1         = '@0V@'
*          text_button_2         = 'Cancelar'
*          icon_button_2         = '@0W@'
*          default_button        = '1'
*          display_cancel_button = ' '
*        IMPORTING
*          answer                = resposta.
*
*      IF resposta EQ c_1.

    LOOP AT tl_0008.

      IF  tl_0008-status = '3'.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'O Romaneio está em processo de estorno'
                                                'no sistema Rondonline,'
                                                'por favor aguardar!'.
        STOP.
      ENDIF.

********** Cancelamento com STATUS 5
      IF tl_0008-status = '1'.
        CONCATENATE 'O Romaneio' tl_0008-nr_romaneio 'da filial'
         tl_0008-werks 'do Bloco' tl_0008-lgort', está em processo de envio ao sistema Rondonline..... Deseja prosseguir com o Cancelamento?' INTO msg SEPARATED BY space.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = TEXT-i07
            text_question         = msg
            text_button_1         = 'Ok'
            icon_button_1         = '@0V@'
            text_button_2         = 'Cancelar'
            icon_button_2         = '@0W@'
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = resposta.

        IF resposta NE c_1.
          REFRESH: tl_0008.
          MESSAGE s836(sd) WITH 'Ação cancelada'.
        ELSE.
          "DESCRIBE TABLE tl_0008 LINES wl_cont.
          wl_cont = REDUCE i( INIT x = 0 FOR wa IN tl_0008 WHERE ( status = '1' ) NEXT x = x + 1 ).
          IF wl_cont GT 0.
            wl_cont_aux = wl_cont.
            CONCATENATE 'Serão cancelados' wl_cont_aux 'fardos de venda.' INTO msg SEPARATED BY space.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = TEXT-i07
                text_question         = msg
                text_button_1         = 'Ok'
                icon_button_1         = '@0V@'
                text_button_2         = 'Cancelar'
                icon_button_2         = '@0W@'
                default_button        = '1'
                display_cancel_button = ' '
              IMPORTING
                answer                = resposta.

            IF resposta EQ c_1.

*-CS2022000332-#78518 -30.04.2022-JT-inicio
              PERFORM f_verifica_intercompany TABLES tl_0008
                                            CHANGING l_erro.
              IF l_erro = abap_true.
                REFRESH: tl_0008.
                MESSAGE s836(sd) WITH 'Ação cancelada'.
                CONTINUE.
              ENDIF.
*-CS2022000332-#78518 -30.04.2022-JT-fim

              LOOP AT tl_0008.
                tl_0008-motorista   = space.
                tl_0008-nr_romaneio = space.
                tl_0008-vbeln       = space.
                tl_0008-vbeln_vf    = space.
                tl_0008-placa_cav   = space.
                tl_0008-nfnum       = space.
                tl_0008-kunnr       = space.
                tl_0008-integ_rondonline   = space.
                tl_0008-dt_inicial_integ = space.
                MODIFY tl_0008.
              ENDLOOP.

              MODIFY zmmt0008 FROM TABLE tl_0008.
              MESSAGE s836(sd) WITH 'Foram cancelados '
                                    wl_cont_aux
                                    ' fardos de venda(s)!'.
              "REFRESH: tl_0008.
              DELETE tl_0008 WHERE status = '1'.

            ELSE.
              REFRESH: tl_0008.
              MESSAGE s836(sd) WITH 'Ação cancelada'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
********** Cancelamento com STATUS 5
      IF tl_0008-status = '5'.
        CONCATENATE 'O Romaneio' tl_0008-nr_romaneio 'da filial'
         tl_0008-werks 'do Bloco' tl_0008-lgort', não foi integrado com o sistema Rondonline... Deseja prosseguir com o Cancelamento?' INTO msg SEPARATED BY space.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = TEXT-i07
            text_question         = msg
            text_button_1         = 'Ok'
            icon_button_1         = '@0V@'
            text_button_2         = 'Cancelar'
            icon_button_2         = '@0W@'
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = resposta.

        IF resposta NE c_1.
          REFRESH: tl_0008.
          MESSAGE s836(sd) WITH 'Ação cancelada'.
        ELSE.
          "DESCRIBE TABLE tl_0008 LINES wl_cont.
          wl_cont = REDUCE i( INIT x = 0 FOR wa IN tl_0008 WHERE ( status = '5' ) NEXT x = x + 1 ).
          IF wl_cont GT 0.
            wl_cont_aux = wl_cont.
            CONCATENATE 'Serão cancelados' wl_cont_aux 'fardos de venda.' INTO msg SEPARATED BY space.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = TEXT-i07
                text_question         = msg
                text_button_1         = 'Ok'
                icon_button_1         = '@0V@'
                text_button_2         = 'Cancelar'
                icon_button_2         = '@0W@'
                default_button        = '1'
                display_cancel_button = ' '
              IMPORTING
                answer                = resposta.

            IF resposta EQ c_1.
*-CS2022000332-#78518 -30.04.2022-JT-inicio
              PERFORM f_verifica_intercompany TABLES tl_0008
                                            CHANGING l_erro.
              IF l_erro = abap_true.
                REFRESH: tl_0008.
                MESSAGE s836(sd) WITH 'Ação cancelada'.
                CONTINUE.
              ENDIF.
*-CS2022000332-#78518 -30.04.2022-JT-fim

              LOOP AT tl_0008.
                tl_0008-motorista   = space.
                tl_0008-nr_romaneio = space.
                tl_0008-vbeln       = space.
                tl_0008-vbeln_vf    = space.
                tl_0008-placa_cav   = space.
                tl_0008-nfnum       = space.
                tl_0008-kunnr       = space.
                tl_0008-integ_rondonline   = space.
                tl_0008-dt_inicial_integ = space.
                MODIFY tl_0008.
              ENDLOOP.

              MODIFY zmmt0008 FROM TABLE tl_0008.
              MESSAGE s836(sd) WITH 'Foram cancelados '
                                    wl_cont_aux
                                    ' fardos de venda(s)!'.
              "REFRESH: tl_0008.
              DELETE tl_0008 WHERE status = '5'.

            ELSE.
              REFRESH: tl_0008.
              MESSAGE s836(sd) WITH 'Ação cancelada'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

********** Cancelamento com STATUS 2
      IF tl_0008-status = '2'.
        "DESCRIBE TABLE tl_0008 LINES wl_cont.

        wl_cont = REDUCE i( INIT x = 0 FOR wa IN tl_0008 WHERE ( status = '2' ) NEXT x = x + 1 ).

        IF wl_cont GT 0.
          wl_cont_aux = wl_cont.
          CONCATENATE 'Serão cancelados' wl_cont_aux 'fardos de venda.' INTO msg SEPARATED BY space.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = TEXT-i07
              text_question         = msg
              text_button_1         = 'Ok'
              icon_button_1         = '@0V@'
              text_button_2         = 'Cancelar'
              icon_button_2         = '@0W@'
              default_button        = '1'
              display_cancel_button = ' '
            IMPORTING
              answer                = resposta.

          IF resposta NE c_1.
            REFRESH: tl_0008.
            MESSAGE s836(sd) WITH 'Ação cancelada'.
          ELSE.
*-CS2022000332-#78518 -30.04.2022-JT-inicio
            PERFORM f_verifica_intercompany TABLES tl_0008
                                          CHANGING l_erro.
            IF l_erro = abap_true.
              REFRESH: tl_0008.
              MESSAGE s836(sd) WITH 'Ação cancelada'.
              CONTINUE.
            ENDIF.
*-CS2022000332-#78518 -30.04.2022-JT-fim

            LOOP AT tl_0008 WHERE status = '2'.
              tl_0008-status = '3'.
              tl_0008-dt_inicial_integ = sy-datum.
              MODIFY tl_0008.
            ENDLOOP.

            MODIFY zmmt0008 FROM TABLE tl_0008.
            COMMIT WORK.

            CLEAR: lit_selection.

            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_WERKS'.          "Option name
            lwa_selection-kind     = 'S'.                "S= select options P=Parameters
            lwa_selection-sign     = 'I'.                "Sign
            lwa_selection-option   = 'EQ'.               "Option
            lwa_selection-low      = tl_0008-werks . "Value

            APPEND lwa_selection TO lit_selection.

            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_LGORT'.
            lwa_selection-kind     = 'S'.
            lwa_selection-sign     = 'I'.
            lwa_selection-option   = 'EQ'.
            lwa_selection-low      = tl_0008-lgort .

            APPEND lwa_selection TO lit_selection.

            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_VBN_VF'.
            lwa_selection-kind     = 'S'.
            lwa_selection-sign     = 'I'.
            lwa_selection-option   = 'EQ'.
            lwa_selection-low      = tl_0008-vbeln_vf .

            APPEND lwa_selection TO lit_selection.

            CLEAR lwa_selection.
            lwa_selection-selname  = 'S_PLCAV'.
            lwa_selection-kind     = 'S'.
            lwa_selection-sign     = 'I'.
            lwa_selection-option   = 'EQ'.
            lwa_selection-low      = tl_0008-placa_cav .

            APPEND lwa_selection TO lit_selection.

            SELECT SINGLE *
              FROM tvarvc INTO @DATA(lwa_tvarv_integra_rondoline)
             WHERE name EQ 'ZMM0027_INTEGRA_RONDOLINE'.

            IF sy-subrc EQ 0.
              SUBMIT zmmr162 WITH SELECTION-TABLE lit_selection WITH s_acao EQ 'E' AND RETURN.
            ENDIF.

            "REFRESH: tl_0008.
            DELETE tl_0008 WHERE status = '3'.

          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

**** PBI - 64946 - Inicio

*    LOOP AT tl_0008.
*
*      tl_0008-motorista   = space.
*      tl_0008-nr_romaneio = space.
*      tl_0008-vbeln       = space.
*      tl_0008-vbeln_vf    = space.
*      tl_0008-placa_cav   = space.
*      tl_0008-nfnum       = space.
*      tl_0008-kunnr       = space.
*      MODIFY tl_0008.
*    ENDLOOP.
*
*    MODIFY zmmt0008 FROM TABLE tl_0008.
*
*    MESSAGE s836(sd) WITH 'Foram cancelados '
*                          wl_cont_aux
*                          ' fardos de venda(s)!'.
*  ELSE.
*    REFRESH: tl_0008.
*    MESSAGE s836(sd) WITH 'Ação cancelada'.
*  ENDIF.
**** PBI - 64946  - Fim
ENDFORM.                    " SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM f_verifica_intercompany TABLES pl_0008 STRUCTURE zmmt0008
                           CHANGING p_erro.

  FREE: p_erro.

  READ TABLE pl_0008 INTO DATA(lwa_zmmt0008_tmp) INDEX 1.
  CHECK sy-subrc EQ 0 AND lwa_zmmt0008_tmp-kunnr IS NOT INITIAL.

  CHECK pl_0008[] IS NOT INITIAL.

  SELECT ch_referencia, id_cli_dest
    FROM zsdt0001
    INTO TABLE @DATA(t_0001)
   WHERE nr_romaneio  = @p_roman
     AND nr_safra     = @p_safra
     AND branch       = @p_werks
     AND tp_movimento = 'S'.

  CHECK t_0001[] IS NOT INITIAL.

  SELECT kunnr, ktokd
    FROM kna1
    INTO TABLE @DATA(t_kna1)
     FOR ALL ENTRIES IN @pl_0008
   WHERE kunnr   = @pl_0008-kunnr.

  READ TABLE t_kna1 INTO DATA(w_kna1) WITH KEY ktokd = 'ZCIC'.
  CHECK sy-subrc = 0.

  READ TABLE t_0001 INTO DATA(w_0001) INDEX 1.
  CHECK sy-subrc = 0.

  CHECK w_kna1-kunnr+6(4) NE p_werks.

  SELECT *
    INTO TABLE @DATA(t_0008)
    FROM zmmt0008
     FOR ALL ENTRIES IN @pl_0008
   WHERE werks  = @w_kna1-kunnr+6(4)
     AND lgort  = @pl_0008-lgort
     AND charg  = @pl_0008-charg
     AND safra  = @pl_0008-safra
     AND menge  = @pl_0008-menge.

  LOOP AT t_0008 INTO DATA(w_0008) WHERE nr_romaneio IS NOT INITIAL.
    EXIT.
  ENDLOOP.

  IF sy-subrc = 0.
    p_erro = abap_true.

    CONCATENATE 'Os dados replicados do lote' w_0008-charg 'para o centro' w_0008-werks
           INTO l_mesg1 SEPARATED BY space.
    l_mesg2 = 'já foram utilidos nos romaneios, a serem exibidos.'.
    l_mesg3 = 'Solicite o Cancel.dos Romaneios para cancelar replicação de dados'.

    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT_LO'
      EXPORTING
        titel        = 'Cancelamento Romaneio'
        textline1    = l_mesg1
        textline2    = l_mesg2
        textline3    = l_mesg3
        start_column = 25
        start_row    = 6.

    CALL FUNCTION 'ZSD_LOG_CANCEL_ROMANEIO'
      TABLES
        t_dados = t_0008.
  ELSE.
    LOOP AT t_0008 INTO w_0008.
      DELETE FROM zmmt0008 WHERE werks = w_0008-werks
                             AND lgort = w_0008-lgort
                             AND charg = w_0008-charg
                             AND safra = w_0008-safra.
    ENDLOOP.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_dados .
  LOOP AT tg_vbrp INTO wg_vbrp.
    READ TABLE tg_lin INTO wg_lin
      WITH KEY refkey = wg_vbrp-refkey.

    IF sy-subrc IS INITIAL.
      READ TABLE tg_bnfe INTO wg_bnfe
        WITH KEY docnum = wg_lin-docnum.

      IF sy-subrc IS INITIAL.

      ENDIF.
      READ TABLE tg_zsdt0001 INTO wg_zsdt0001
        WITH KEY doc_rem = wg_vbrp-vgbel.

*     READ TABLE TG_ZMMT0008 INTO WG_ZMMT0008
*       WITH KEY
      LOOP AT tg_zmmt0008          INTO wg_zmmt0008.
        MOVE: wg_zmmt0008-werks      TO wg_saida-werks,
              wg_zmmt0008-lgort      TO wg_saida-bloco,
              wg_zmmt0008-charg      TO wg_saida-fardo,
              p_roman                TO wg_saida-nr_romaneio,
              wg_bnfe-nfnum9         TO wg_saida-nfnum,
              wg_zsdt0001-vbeln      TO wg_saida-vbeln,
              wg_vbrp-vbeln          TO wg_saida-vbeln_vf,
              wg_vbrp-matnr          TO wg_saida-matnr,
              p_placa                TO wg_saida-placa_cav,
              wg_zmmt0008-menge      TO wg_saida-menge,
              wg_zmmt0008-werks_orig TO wg_saida-werks_orig,
              wg_zmmt0008-lgortr     TO wg_saida-lgortr, "*-S4H-US 123905-25.09.2023-JT
              wg_zmmt0008-safra      TO wg_saida-safra,  "*-S4H-US 123905-25.09.2023-JT
              wg_zmmt0008-mblnr      TO wg_saida-mblnr,  "*-S4H-US 123905-25.09.2023-JT
              wg_zmmt0008-mjahr      TO wg_saida-mjahr,  "*-S4H-US 123905-25.09.2023-JT
              wg_zmmt0008-dados_contingencia TO wg_saida-dados_contingencia, "*-S4H-US 123905-25.09.2023-JT
              wg_zmmt0008-tipo_fardo TO wg_saida-tipo_fardo,
              wg_zmmt0008-cd_sai     TO wg_saida-cd_sai. "*-S4H-US 123905-25.09.2023-JT
*              wg_zmmt0008-charg TO wg_saida-charg,
*              wg_zmmt0008-lgort TO wg_saida-lgort.

        APPEND wg_saida TO tg_saida.
        CLEAR: wg_saida.
      ENDLOOP.
      CLEAR: wg_vbrp, wg_lin, wg_zsdt0001, wg_bnfe.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_dados.

  wg_layout-box_fieldname = 'MARK'. " fieldname for checkbox
  wg_layout-box_tabname   = 'TG_SAIDA'." tabname for checkbox

  PERFORM definir_eventos.
  PERFORM montar_layout.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = v_report
      i_callback_user_command  = 'XUSER_COMMAND'
      i_callback_pf_status_set = 'XPF_STATUS'
      it_fieldcat              = estrutura[]
      i_save                   = 'A'
      is_layout                = wg_layout
      it_events                = events
      is_print                 = t_print
    TABLES
      t_outtab                 = tg_saida.

ENDFORM.                    " IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.
  PERFORM f_carregar_eventos USING:
*                                 SLIS_EV_USER_COMMAND 'XUSER_COMMAND',
                                 slis_ev_top_of_page  'XTOP_OF_PAGE'.

ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos
*&---------------------------------------------------------------------*
*&      Form  XPF_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM xpf_status USING o_extab TYPE slis_t_extab.
  SET PF-STATUS 'STANDARD_FULLSCREEN'.

ENDFORM.                    " XPF_STATUS
*&---------------------------------------------------------------------*
*&      Form  XUSER_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM xuser_command USING ucomm LIKE sy-ucomm
                         selfield TYPE slis_selfield.
  CASE ucomm.
**> Exibi tabela de motivos
    WHEN '&PROC'.
      PERFORM atualiza_zmmt0008.

  ENDCASE.
ENDFORM.                    " XUSER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout.
  PERFORM montar_estrutura USING:
        1 'ZMMT0008'  'LGORT' 'TG_SAIDA' 'BLOCO'  ' '  ' ' ,
        2 'ZMMT0008'  'CHARG' 'TG_SAIDA' 'FARDO'  ' '  ' ' ,
        3 'VBRP'      'MATNR' 'TG_SAIDA' 'MATNR'  ' '  ' ' ,
        4 'ZMMT0008'  'MENGE' 'TG_SAIDA' 'MENGE'  ' '  ' ' .


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen).

  CLEAR wa_estrutura.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " montar_estrutura

*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = 'CLARO_50'.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaves.

  v_report = sy-repid.

  PERFORM f_construir_cabecalho USING 'H' space TEXT-002.
  PERFORM f_construir_cabecalho USING 'S' 'Romaneio:' p_roman.
  READ TABLE tg_bnfe INTO wg_bnfe INDEX 1.

  IF wg_bnfe-bukrs IS NOT INITIAL.
    SELECT SINGLE *
      FROM j_1bbranch
       WHERE bukrs  EQ wg_bnfe-bukrs
         AND branch EQ wg_bnfe-branch.

  ENDIF.
  PERFORM f_construir_cabecalho USING 'S' 'Remetente:' j_1bbranch-name.

  READ TABLE tg_kna1 INTO wg_kna1 INDEX 1.
  PERFORM f_construir_cabecalho USING 'S' 'Destinatario:' wg_kna1-name1.

ENDFORM.                    " INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ key text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-key = key.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_ZMMT0008
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_zmmt0008 .
  DATA: tl_0008        TYPE TABLE OF zmmt0008 WITH HEADER LINE,
        w_werks        TYPE zmmt0008-werks,
        l_interc       TYPE c,
        wl_cont        TYPE sy-tabix,
        msg(150),
        resposta,
        wl_cont_aux(6).

  TYPES: tt_selection TYPE STANDARD TABLE OF rsparams.

  DATA:
*     Define internal table and work area for select-options
    l_selection  TYPE rsparams,
    li_selection TYPE tt_selection.

  REFRESH: tl_0008.
  CLEAR: wl_cont, l_interc.

*** CSB - 30.11.2021 - Inicio
  READ TABLE tg_saida INTO wg_saida WITH KEY mark  = c_x
                                             nfnum = 0.
*-BUG 76557 - 18.04.2022 - JT - inicio
* IF wg_saida IS NOT INITIAL.
  IF sy-subrc IS INITIAL.
*   CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
*     EXPORTING
*       titel        = 'Atenção'
*       textline1    = 'Não localizado número da Nota para o romaneio informado,'
*       textline2    = 'verifique se a mesma esta determinada!'
*       start_column = 25
*       start_row    = 6.
    MESSAGE i836(sd)  WITH 'Não localizado número da Nota para o romaneio informado,'
                           'verifique se a mesma esta determinada!'.
    EXIT.
*-BUG 76557 - 18.04.2022 - JT - fim
*   MESSAGE s836(sd) DISPLAY LIKE 'I' WITH 'Não localizado número da Nota para o romaneio informado,'
*                                          'verifique se a mesma esta determinada!'.
*   STOP.
  ENDIF.
*** CSB - 30.11.2021 - Fim

*-CS2022000332-#78518 -30.04.2022-JT-inicio
  SELECT SINGLE kunnr, ktokd
    FROM kna1
    INTO @DATA(w_kna1)
   WHERE kunnr = @p_kunnr.

  IF sy-subrc = 0 AND w_kna1-ktokd = 'ZCIC'.
    l_interc = abap_true.

    SELECT *
      FROM zmmt0008
      INTO TABLE @DATA(t_0008)
       FOR ALL ENTRIES IN @tg_saida
     WHERE werks = @w_kna1-kunnr+6(4)
       AND lgort = @tg_saida-bloco
       AND charg = @tg_saida-fardo
       AND safra = @tg_saida-safra.
  ENDIF.
*-CS2022000332-#78518 -30.04.2022-JT-fim

*-CS2023000189-18.05.2023-#108750-JT-inicio
  l_erro = abap_false.

  LOOP AT tg_saida INTO wg_saida WHERE mark EQ c_x.
    SELECT SINGLE *
      INTO @DATA(w_0330)
      FROM zsdt0330
     WHERE vbeln     = @wg_saida-vbeln
       AND werks     = @wg_saida-werks
       AND lgort     = @wg_saida-bloco
       AND acharg    = @wg_saida-fardo
       AND cancelado = @abap_false.

    IF sy-subrc = 0.
      l_erro = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF l_erro = abap_true.
    MESSAGE s024(sd) WITH 'Este Fardo já está sendo Tratado pelo '
                          'Processamento Automático!' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
*-CS2023000189-18.05.2023-#108750-JT-fim
  DATA(count_registros) = 0.

  LOOP AT tg_saida INTO wg_saida
     WHERE mark EQ c_x.
    ADD 1 TO count_registros.
*    CALL FUNCTION 'ENQUEUE_EZMMT0008'
*      EXPORTING
*        mandt          = sy-mandt
*        werks          = wg_saida-werks
*        lgort          = wg_saida-lgort
*        charg          = wg_saida-charg
*      EXCEPTIONS
*        foreign_lock   = 1
*        system_failure = 2
*        OTHERS         = 3.
*
*    CHECK ( sy-subrc EQ 0 ).

    CLEAR tl_0008.

    SELECT SINGLE *
      FROM zmmt0008 INTO @DATA(lwa_zmmt0008_exists)
     WHERE werks EQ @wg_saida-werks
       AND lgort EQ @wg_saida-bloco
       AND charg EQ @wg_saida-fardo
       AND safra EQ @wg_saida-safra.

    IF sy-subrc EQ 0.
      MOVE: lwa_zmmt0008_exists-dt_registro   TO tl_0008-dt_registro,
            lwa_zmmt0008_exists-hr_registro   TO tl_0008-hr_registro.
    ENDIF.

    MOVE: wg_saida-werks       TO w_werks.

    MOVE: sy-mandt             TO tl_0008-mandt,
          wg_saida-werks       TO tl_0008-werks,
          wg_saida-bloco       TO tl_0008-lgort,
          wg_saida-fardo       TO tl_0008-charg,
          wg_saida-safra       TO tl_0008-safra,  "*-S4H-US 123905-25.09.2023-JT
          wg_saida-nr_romaneio TO tl_0008-nr_romaneio,
          wg_saida-nfnum       TO tl_0008-nfnum,
          wg_saida-vbeln       TO tl_0008-vbeln,
          wg_saida-vbeln_vf    TO tl_0008-vbeln_vf,
          wg_saida-placa_cav   TO tl_0008-placa_cav,
          wg_saida-menge       TO tl_0008-menge,
          wg_saida-werks_orig  TO tl_0008-werks_orig,
          wg_saida-matnr       TO tl_0008-matnr, "*-S4H-US 123905-25.09.2023-JT
          wg_saida-lgortr      TO tl_0008-lgortr, "*-S4H-US 123905-25.09.2023-JT
          wg_saida-mblnr       TO tl_0008-mblnr,  "*-S4H-US 123905-25.09.2023-JT
          wg_saida-mjahr       TO tl_0008-mjahr,  "*-S4H-US 123905-25.09.2023-JT
          wg_saida-dados_contingencia TO tl_0008-dados_contingencia, "*-S4H-US 123905-25.09.2023-JT
          wg_saida-cd_sai      TO tl_0008-cd_sai, "*-S4H-US 123905-25.09.2023-JT
          wg_saida-tipo_fardo  TO tl_0008-tipo_fardo, "*-S4H-US 123905-25.09.2023-JT
          p_motor              TO tl_0008-motorista,
          p_kunnr              TO tl_0008-kunnr.

* PBI - 64946 - Inicio - CSB
    tl_0008-dt_inicial_integ = sy-datum.
    tl_0008-status = '1'.
* PBI - 64946 - Fim - CSB

    APPEND tl_0008.

    CLEAR l_selection.
    l_selection-selname  = 'S_WERKS'.          "Option name
    l_selection-kind     = 'S'.                "S= select options P=Parameters
    l_selection-sign     = 'I'.                "Sign
    l_selection-option   = 'EQ'.               "Option
    l_selection-low      = w_werks .           "*-CS2022000332-#78518 -30.04.2022-JT-inicio

    APPEND l_selection TO li_selection.

    CLEAR l_selection.
    l_selection-selname  = 'S_LGORT'.
    l_selection-kind     = 'S'.
    l_selection-sign     = 'I'.
    l_selection-option   = 'EQ'.
    l_selection-low      = tl_0008-lgort .

    APPEND l_selection TO li_selection.

    CLEAR l_selection.
    l_selection-selname  = 'S_VBN_VF'.
    l_selection-kind     = 'S'.
    l_selection-sign     = 'I'.
    l_selection-option   = 'EQ'.
    l_selection-low      = tl_0008-vbeln_vf .

    APPEND l_selection TO li_selection.

    CLEAR l_selection.
    l_selection-selname  = 'S_PLCAV'.
    l_selection-kind     = 'S'.
    l_selection-sign     = 'I'.
    l_selection-option   = 'EQ'.
    l_selection-low      = tl_0008-placa_cav .

    APPEND l_selection TO li_selection.

*-CS2022000332-#78518 -30.04.2022-JT-inicio
    IF l_interc = abap_true AND w_kna1-kunnr+6(4) NE w_werks.
      READ TABLE t_0008 INTO DATA(w_0008) WITH KEY werks = w_kna1-kunnr+6(4)
                                                   lgort = wg_saida-bloco
                                                   charg = wg_saida-fardo
                                                   safra = wg_saida-safra.
      IF ( sy-subrc <> 0 ) OR ( sy-subrc = 0 AND w_0008-nr_romaneio IS INITIAL ).
        CLEAR tl_0008.

        MOVE: lwa_zmmt0008_exists-dt_registro   TO tl_0008-dt_registro,
              lwa_zmmt0008_exists-hr_registro   TO tl_0008-hr_registro.

        MOVE:  sy-mandt          TO tl_0008-mandt,
               w_kna1-kunnr+6(4) TO tl_0008-werks,
               wg_saida-bloco    TO tl_0008-lgort,
               wg_saida-fardo    TO tl_0008-charg,
               wg_saida-safra    TO tl_0008-safra,  "*-S4H-US 123905-25.09.2023-JT
               wg_saida-werks    TO tl_0008-werks_orig, "*-CS2022000332-#78803-05.07.2022-JT-inicio
               wg_saida-menge    TO tl_0008-menge,
               wg_saida-matnr    TO tl_0008-matnr, "*-S4H-US 123905-25.09.2023-JT
               wg_saida-lgortr   TO tl_0008-lgortr, "*-S4H-US 123905-25.09.2023-JT
*              wg_saida-mblnr    TO tl_0008-mblnr,  "*-S4H-US 123905-25.09.2023-JT
*              wg_saida-mjahr    TO tl_0008-mjahr,  "*-S4H-US 123905-25.09.2023-JT
               wg_saida-dados_contingencia TO tl_0008-dados_contingencia, "*-S4H-US 123905-25.09.2023-JT
               wg_saida-tipo_fardo TO tl_0008-tipo_fardo,
               wg_saida-cd_sai     TO tl_0008-cd_sai.
*              sy-datum          TO tl_0008-dt_inicial_integ,
*              '1'               TO tl_0008-status.
        APPEND tl_0008.

        CLEAR l_selection.
        l_selection-selname = 'S_WERKS'.          "Option name
        l_selection-kind    = 'S'.                "S= select options P=Parameters
        l_selection-sign    = 'I'.                "Sign
        l_selection-option  = 'EQ'.               "Option
        l_selection-low     = tl_0008-werks . "   Value
        APPEND l_selection TO li_selection.
      ENDIF.
    ENDIF.
*-CS2022000332-#78518 -30.04.2022-JT-fim

  ENDLOOP.

  IF count_registros GT 0.
    wl_cont_aux = count_registros.
    CONCATENATE 'Serão gerados' wl_cont_aux 'fardos de venda.' INTO msg SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = TEXT-i06
        text_question         = msg
        text_button_1         = 'Ok'
        icon_button_1         = '@0V@'
        text_button_2         = 'Cancelar'
        icon_button_2         = '@0W@'
        default_button        = '1'
        display_cancel_button = ' '
      IMPORTING
        answer                = resposta.

    IF resposta EQ c_1.
      MODIFY zmmt0008 FROM TABLE tl_0008.

* BUG 53371 - Inicio -
* SUBMIT zmmr162 AND RETURN.

      SORT li_selection BY selname kind sign option low  .

      DELETE ADJACENT DUPLICATES FROM li_selection COMPARING ALL FIELDS.

      SELECT SINGLE *
        FROM tvarvc INTO @DATA(lwa_tvarv_integra_rondoline)
       WHERE name EQ 'ZMM0027_INTEGRA_RONDOLINE'.

      IF sy-subrc EQ 0.
        SUBMIT zmmr162 WITH SELECTION-TABLE li_selection WITH s_acao EQ 'I' AND RETURN. " PBI - 64946 - CSB
      ENDIF.
* BUG 53371 - Fim -


      MESSAGE s836(sd) WITH 'Foram Gerados '
                            wl_cont_aux
                            ' fardos de venda(s)!'.
      PERFORM  seleciona_dados_email.


*      LOOP AT tg_saida INTO wg_saida
*              WHERE mark EQ c_x.
*
*        CALL FUNCTION 'DEQUEUE_EZMMT0008'
*          EXPORTING
*            mandt          = sy-mandt
*            werks          = wg_saida-werks
*            lgort          = wg_saida-lgort
*            charg          = wg_saida-charg
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.
*      ENDLOOP.

      LEAVE TO SCREEN 0.
    ELSE.
      REFRESH: tl_0008.
      MESSAGE s836(sd) WITH 'Ação cancelada'.

    ENDIF.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Você precisa selecionar pelo menos um fardo!'.
  ENDIF.
ENDFORM.                    " ATUALIZA_ZMMT0008
*&---------------------------------------------------------------------*
*&      Form  IMPRIME_SMARTFORMS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_smartforms .
  DATA: vl_form    TYPE tdsfname,
        vl_name    TYPE rs38l_fnam,
        vl_safra   TYPE char4,
        sl_rodape  TYPE zmmr0001_rod,
        tl_fardos1 TYPE TABLE OF zmms004 WITH HEADER LINE,
        tl_fardos2 TYPE TABLE OF zmms004,
        tl_fardos3 TYPE TABLE OF zmms004,
        tl_fardos4 TYPE TABLE OF zmms004,
        wl_cont    TYPE sy-tabix,
        wl_menge   TYPE zmmt0008-menge,
        tg_total   TYPE TABLE OF zmms006 WITH HEADER LINE,
        wl_tipo    TYPE mara-normt,
        wl_flag.
*

  vl_form = 'ZMMS002'.
*
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  SORT: tg_fardos BY tipo.
*  LOOP AT tg_fardos.
*    AT NEW tipo.
*      tL_fardos1[] = tg_fardos[].
*      DELETE tL_fardos1 WHERE tipo NE tg_fardos-tipo.
*      LOOP AT tL_fardos1.
*        ADD 1 TO wl_cont.
*        ADD tL_fardos1-peso TO wl_menge.
*
*        AT LAST.
*          MOVE: wl_cont        TO  tg_total-total,
*                tg_fardos-tipo TO tg_total-tipo,
*                wl_menge       TO tg_total-peso.
*
*          APPEND tg_total.
*          CLEAR: wl_menge, wl_cont.
*        ENDAT.
*      ENDLOOP.
*    ENDAT.
*  ENDLOOP.

  LOOP AT tg_fardos.
    MOVE:                    1 TO  tg_total-total,
                tg_fardos-tipo TO tg_total-tipo,
                tg_fardos-peso TO tg_total-peso.
    COLLECT tg_total.
  ENDLOOP.


  CALL FUNCTION vl_name
    EXPORTING
      header           = wg_header
    TABLES
      tg_fardos        = tg_fardos
      tg_fardos1       = tl_fardos1
      tg_fardos2       = tl_fardos2
      tg_fardos3       = tl_fardos3
      tg_fardos4       = tl_fardos4
      tg_total         = tg_total
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " IMPRIME_SMARTFORMS

FORM seleciona_dados_email.
  SELECT * FROM zmmt0008 INTO TABLE @DATA(it_0008)
     WHERE werks       EQ @p_werks
       AND lgort       EQ @p_bloco
       AND nr_romaneio EQ @p_roman
       AND safra       EQ @p_safra.

  IF sy-subrc IS INITIAL.
    READ TABLE it_0008 INTO DATA(wa_0008) INDEX 1.
    wg_excel-veiculo   = wa_0008-placa_cav.
    wg_excel-romaneio  = wa_0008-nr_romaneio.
    wg_excel-motorista = wa_0008-motorista.
    wg_excel-bloco     = wa_0008-lgort.
    SELECT SINGLE name1  FROM kna1
      INTO wg_excel-cliente
       WHERE kunnr EQ wa_0008-kunnr.

    SELECT SINGLE *
     FROM zsdt0001
      WHERE vbeln EQ wa_0008-vbeln
        AND tp_movimento EQ c_s
        AND nr_romaneio  EQ p_roman
        AND branch       EQ p_werks
        AND vbeln        EQ wa_0008-vbeln.
    IF sy-subrc IS INITIAL.
      wg_excel-safra = zsdt0001-nr_safra.

      SELECT SINGLE * FROM zsdt0066 INTO @DATA(wa_zsdt0066)
        WHERE vbeln EQ @zsdt0001-vbeln.

* Envio de Romaneio de Saída (Algodoeira)
      IF sy-subrc IS INITIAL. "busca dados baseado na formação de lote
        CLEAR wa_zsdt0045.
        SELECT SINGLE zseq_inst objek objecttable booking instrucao FROM zsdt0045 INTO wa_zsdt0045
          WHERE objek      EQ wa_zsdt0066-nro_sol_ov
            AND instrucao  EQ wa_zsdt0066-instrucao.

        SELECT * FROM zmmt0126 INTO TABLE @DATA(t_zmmt0126)
       WHERE kunnr EQ @wa_zsdt0066-lentrega.

        wg_excel-instrucao = wa_zsdt0066-instrucao.

      ELSE. "busca dados baseado na venda ZFEX
        SELECT SINGLE * FROM zsdt0053 INTO @DATA(wa_zsdt0053)
         WHERE vbeln EQ @zsdt0001-vbeln.

        CLEAR wa_zsdt0045.
        SELECT SINGLE zseq_inst objek objecttable booking instrucao tamanho_fardo FROM zsdt0045 INTO wa_zsdt0045
          WHERE objek      EQ wa_zsdt0053-nro_sol_ov
            AND instrucao  EQ wa_zsdt0053-instrucao.

        wg_excel-instrucao = wa_zsdt0053-instrucao.

      ENDIF.

**    Destinatario
      SELECT SINGLE name1
        FROM kna1
        INTO wg_excel-terminal
         WHERE kunnr EQ zsdt0001-id_cli_dest.

      SELECT SINGLE *
        FROM j_1bnflin
         WHERE refkey EQ wa_0008-vbeln_vf.
      IF sy-subrc IS INITIAL.
        SELECT SINGLE *
          FROM j_1bnfe_active
           WHERE docnum EQ j_1bnflin-docnum.

        IF sy-subrc IS INITIAL.
**          Nota Fiscal
          wg_excel-nfnum = j_1bnfe_active-nfnum9.
          SELECT SINGLE *
            FROM j_1bbranch
             WHERE bukrs  EQ j_1bnfe_active-bukrs
               AND branch EQ j_1bnfe_active-branch.
** Remetente
          wg_excel-remetente = j_1bbranch-name.
          wg_excel-stcd1     = j_1bbranch-stcd1.

          SELECT SINGLE butxt    FROM t001
            INTO wg_excel-empresa
             WHERE bukrs EQ j_1bnfe_active-bukrs.
        ENDIF.
      ENDIF.
    ENDIF.

    SELECT vbeln vgbel matnr
      FROM vbrp  INTO TABLE tg_vbrp
       WHERE vbeln EQ wa_0008-vbeln_vf.

    IF sy-subrc IS INITIAL.
      SELECT matnr normt
        FROM mara
        INTO TABLE tg_mara
        FOR ALL ENTRIES IN tg_vbrp
         WHERE matnr EQ tg_vbrp-matnr.
    ENDIF.

    LOOP AT it_0008 INTO wa_0008.
      LOOP AT tg_mara INTO wg_mara.

        "Projeto Reestruturação Algodao 2024
*        SELECT SINGLE *  FROM zppt0002 INTO @DATA(wa_0002)
*         WHERE acharg EQ @wa_0008-charg
*           AND lgort = @wa_0008-lgort
*          AND werks = @wa_0008-werks.
        ""Projeto Reestruturação Algodao 2024
*** Fim    - CS2021000727 Melhorias na ZMM0027 - JMONTEIRO - 11/10/2021

        "Projeto Reestruturação Algodao 2024
*        IF wa_0002 IS NOT INITIAL.
*          SELECT SINGLE * FROM zppt0004 INTO @DATA(wa_0004)
*            WHERE werks EQ @wa_0002-werks
*            AND   verid EQ @wa_0002-verid.
*        ENDIF.
        "Projeto Reestruturação Algodao 2024

        CONDENSE wa_0008-charg NO-GAPS.
*** Inicio - CS2021000727 Melhorias na ZMM0027 - JMONTEIRO - 11/10/2021
*        wg_fardos_exc-fardos  = wa_0008-charg+2.
        wg_fardos_exc-fardos  = |'{ wa_0008-cd_sai }|. "Projeto Reestruturação Algodao 2024
*** Fim    - CS2021000727 Melhorias na ZMM0027 - JMONTEIRO - 11/10/2021
        wg_fardos_exc-tipo    = wg_mara-normt(5).
        wg_fardos_exc-peso    = wa_0008-menge.
        wg_fardos_exc-tamanho = wa_0008-tipo_fardo.

        IF wg_fardos_exc-tamanho IS INITIAL.
          wg_fardos_exc-tamanho = wa_zsdt0045-tamanho_fardo.
        ENDIF.

        APPEND wg_fardos_exc TO tg_fardos_exc.
      ENDLOOP.
      CLEAR: wa_0008.
    ENDLOOP.

    LOOP AT it_0008 INTO wa_0008.
      CONDENSE wa_0008-charg NO-GAPS.
      wg_excel-qtd_fardos  = wg_excel-qtd_fardos + 1.
      wg_excel-peso_total  = wg_excel-peso_total  + wa_0008-menge.
      CLEAR: wa_0008.
    ENDLOOP.
    APPEND wg_excel  TO tg_excel.
  ENDIF.

  SELECT *
    FROM zmail INTO TABLE it_zmail
    WHERE werks EQ p_werks
    AND tcode   EQ 'ZMM0027'.

  LOOP AT t_zmmt0126 INTO DATA(w_zmmt0126).

    IF w_zmmt0126-email IS NOT INITIAL.

      wa_zmail-email  = w_zmmt0126-email.
      wa_zmail-usuario = w_zmmt0126-usnam.

      APPEND wa_zmail TO it_zmail.
      CLEAR wa_zmail.

    ENDIF.

  ENDLOOP.

*---> 04/07/2023 - Migração S4 - WS
  SORT it_zmail BY  email.
*<--- 04/07/2023 - Migração S4 - WS
  DELETE ADJACENT DUPLICATES FROM it_zmail COMPARING email.

  PERFORM monta_excel.

ENDFORM.

FORM monta_excel.
  DATA: lv_string   TYPE string,
        vqtd_fardos TYPE string,
        vpeso_total TYPE string,
        vpeso       TYPE string,
        vcnpj(18)   TYPE c.

  CONSTANTS:
    gc_tab  TYPE c VALUE cl_bcs_convert=>gc_tab,
    gc_tab2 TYPE c VALUE cl_abap_char_utilities=>vertical_tab,
    gc_crlf TYPE c VALUE cl_bcs_convert=>gc_crlf.

  LOOP AT tg_excel INTO wg_excel.

    vqtd_fardos = wg_excel-qtd_fardos.
    vpeso_total = wg_excel-peso_total.

    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_OUTPUT'
      EXPORTING
        input  = wg_excel-stcd1
      IMPORTING
        output = vcnpj.

    CONCATENATE
    'Empresa:'          gc_tab    wg_excel-empresa        gc_crlf
    'Safra:'            gc_tab    wg_excel-safra          gc_crlf
    'Remetente:'        gc_tab    wg_excel-remetente      gc_crlf
    'CNPJ Remetente'    gc_tab    vcnpj                   gc_crlf
    'Terminal:'         gc_tab    wg_excel-terminal       gc_crlf
    'Cliente:'          gc_tab    wg_excel-cliente        gc_crlf
    'Romaneio:'         gc_tab    wg_excel-romaneio       gc_crlf
    'NFe.F.Lote:'       gc_tab    wg_excel-nfnum          gc_crlf
    'Motorista:'        gc_tab    wg_excel-motorista      gc_crlf
    'Veículo (Placa):'  gc_tab    wg_excel-veiculo        gc_crlf
    'Lote (Bloco):'     gc_tab    wg_excel-bloco          gc_crlf
    'Qtd Fardos:'       gc_tab    vqtd_fardos             gc_crlf
    'Peso Total:'       gc_tab    vpeso_total             gc_crlf
    'Instrução:'        gc_tab    wg_excel-instrucao      gc_crlf
    INTO lv_string.

    CLEAR:  vqtd_fardos, vpeso_total.
  ENDLOOP.

  CONCATENATE
  lv_string       gc_crlf
                  gc_crlf
  'Nr. Fardo'     gc_tab
  'Tipo'          gc_tab
  'Peso'          gc_tab
  'Tamanho'       gc_crlf
  INTO lv_string.

  LOOP AT tg_fardos_exc INTO wg_fardos_exc.

    vpeso =   wg_fardos_exc-peso.
    CONCATENATE
    lv_string
     wg_fardos_exc-fardos   gc_tab
     wg_fardos_exc-tipo     gc_tab
     vpeso                  gc_tab
     wg_fardos_exc-tamanho  gc_crlf
    INTO lv_string.

    CLEAR: wg_fardos_exc, vpeso.
  ENDLOOP.

  CLEAR: size, binary_content.

  TRY.
      cl_bcs_convert=>string_to_solix(
        EXPORTING
          iv_string   = lv_string
          iv_codepage = '4103'
          iv_add_bom  = 'X'
        IMPORTING
          et_solix    = binary_content
          ev_size     = size ).
    CATCH cx_bcs.
      MESSAGE e445(so).
  ENDTRY.

  IF it_zmail[] IS NOT INITIAL.
    PERFORM send.
  ELSE.
    MESSAGE s000(z_mm) WITH 'Romaneio gerado, ' 'mas não foram encontrados ' 'e-mail cadastrados '
    DISPLAY LIKE 'W'.
  ENDIF.
ENDFORM.

FORM send.

  DATA: mailto  TYPE ad_smtpadr,
        subject TYPE so_obj_des.

  DATA: vuser TYPE sy-uname.

  DATA: l_romaneio TYPE c LENGTH 9.

  CLEAR:  main_text, subject, vuser, l_romaneio.

  l_romaneio =  wg_excel-romaneio.
  PACK l_romaneio TO l_romaneio. CONDENSE l_romaneio.
  CONCATENATE 'Romaneio' l_romaneio 'SI' wg_excel-instrucao INTO subject SEPARATED BY space.

  TRY.
      send_request = cl_bcs=>create_persistent( ).

      PERFORM zf_tabela_info_adic.

      document  = cl_document_bcs=>create_document(
         i_type    = 'HTM'"RAW
         i_text    = main_text
         i_subject = subject  ).

      document->add_attachment(
        i_attachment_type    = 'xls'
        i_attachment_subject = subject
        i_attachment_size    = size
*        I_ATT_CONTENT_TEXT   = gt_con
        i_att_content_hex    = binary_content  ).


      send_request->set_document( document ).

      LOOP AT it_zmail INTO wa_zmail.
        IF wa_zmail IS NOT INITIAL.
          CLEAR mailto.

          mailto = wa_zmail-email.

          recipient = cl_cam_address_bcs=>create_internet_address( mailto ).

          send_request->add_recipient( recipient ).
        ENDIF.
      ENDLOOP.

      vuser = sy-uname.
      sy-uname = 'JOBADM'.
      sent_to_all = send_request->send( i_with_error_screen = 'X' ).

      COMMIT WORK.
      sy-uname = vuser.

      IF sent_to_all IS INITIAL.
        MESSAGE s026(so).
      ELSE.
        MESSAGE s022(so).
      ENDIF.


    CATCH cx_bcs INTO bcs_exception.
      MESSAGE i865(so) WITH bcs_exception->error_type.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_VALIDA_SET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_valida_set USING p_value.

  DATA: t_set  TYPE TABLE OF rgsb4,
        wa_set TYPE rgsb4.
  DATA: v_check TYPE c VALUE ''.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      client        = sy-mandt
      setnr         = 'ZMM0027_USER'
      table         = 'ZMMT0005'
      class         = '0000'
      fieldname     = 'USNAM'
    TABLES
      set_values    = t_set
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  READ TABLE t_set INTO DATA(w_set) WITH KEY from = sy-uname.
  IF sy-subrc IS INITIAL.
    p_value = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_call_transaction_email .
  CALL TRANSACTION 'ZMM0090'.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CADASTRO_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_cadastro_email .

  DATA: l_value TYPE c.

  CONSTANTS: c_view TYPE char30 VALUE 'ZVMMT0126',
             c_u    TYPE char1 VALUE 'U',
             c_and  TYPE char3 VALUE 'AND'.

  DATA: gt_seltab   TYPE STANDARD TABLE OF vimsellist,
        g_fieldname TYPE vimsellist-viewfield,
        gt_exclude  TYPE TABLE OF vimexclfun,
        gwa_exclude TYPE vimexclfun.

  PERFORM zf_valida_set USING l_value.

  IF l_value IS NOT INITIAL.

    FREE MEMORY ID 'SEL'.

    CASE 'X'.
      WHEN p_um.
        v_init = 1.
      WHEN p_dois.
        v_init = 2.
      WHEN p_tres.
        v_init = 3.
      WHEN p_quatro.
        v_init = 4.
      WHEN OTHERS.
        v_input = '4'.
    ENDCASE.

    EXPORT v_init TO MEMORY ID 'SEL'.

    g_fieldname = 'KUNNR'.
    CALL FUNCTION 'VIEW_RANGETAB_TO_SELLIST'
      EXPORTING
        fieldname          = g_fieldname
        append_conjunction = c_and
      TABLES
        sellist            = gt_seltab
        rangetab           = p_local.

    CALL FUNCTION 'VIEW_MAINTENANCE_CALL'
      EXPORTING
        action      = c_u
        view_name   = c_view
      TABLES
        dba_sellist = gt_seltab.

  ELSE.
    MESSAGE s000(z_mm) WITH 'Usuário sem autorização! ' 'Contactar equipe Agro corporativo.'
    DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_TABELA_INFO_ADIC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_tabela_info_adic .

  DATA: gs_main_text LIKE LINE OF main_text.

  MOVE '<BR>' TO gs_main_text-line.
  APPEND gs_main_text TO main_text.
  CLEAR gs_main_text.

  CONCATENATE 'Segue em anexo o Romaneio de Saída de Fardos.'(014) '<BR><BR>'
INTO gs_main_text-line SEPARATED BY space.
  APPEND gs_main_text TO main_text.  CLEAR gs_main_text.

  MOVE '<TABLE BORDER=1>' TO gs_main_text-line.
  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

*col1
  MOVE '<TR style="background-color: #96A5AA;"> <TH> Romaneio </TH> ' TO gs_main_text-line.
  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

*col2
  MOVE '<TH> Instrução </TH> ' TO gs_main_text-line.
  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

*col3
  MOVE '<TH> Booking </TH></TR>' TO gs_main_text-line.
  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.


*col1
  IF wg_excel-romaneio IS NOT INITIAL.
    CONCATENATE '<TR> <TD>' wg_excel-romaneio '</TD>' INTO gs_main_text-line.
    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
  ELSE.
    MOVE '<TR> <TD></TD>' TO gs_main_text-line.
    APPEND gs_main_text TO main_text.   CLEAR gs_main_text.
  ENDIF.

*col2
  CONCATENATE '<TD>' wg_excel-instrucao '</TD>' INTO gs_main_text-line.
  APPEND gs_main_text TO main_text.   CLEAR gs_main_text.

*col3
  CONCATENATE '<TD>' wa_zsdt0045-booking '</TD></TR>' INTO gs_main_text-line.
  APPEND gs_main_text TO main_text. CLEAR gs_main_text.


  MOVE '</TABLE>' TO gs_main_text-line.
  APPEND gs_main_text TO main_text.  CLEAR gs_main_text.

  MOVE '<BR>' TO gs_main_text-line.
  APPEND gs_main_text TO main_text.
  CLEAR gs_main_text.

  CONCATENATE '<BR>' 'Atenciosamente'(013) '<BR>'
  INTO gs_main_text-line.
  APPEND gs_main_text TO main_text. CLEAR gs_main_text.

  APPEND '  '     TO main_text.

ENDFORM.
