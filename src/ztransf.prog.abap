*&---------------------------------------------------------------------*
*& Report  ZTRANSF
*&
*&---------------------------------------------------------------------*
*& Gerar remessa a partir do romaneio e pedido de compra
*&
*&---------------------------------------------------------------------*

REPORT  ztransf NO STANDARD PAGE HEADING.

TYPE-POOLS: icon.

*&---------------------------------------------------------------------*
*&      TABLES
*&---------------------------------------------------------------------*
TABLES: ekko, ekpo, mara, lfa1.

*&---------------------------------------------------------------------*
*&      WORK AREA, TABELA INTERNAS
*&---------------------------------------------------------------------*
DATA: BEGIN OF wa_ekko,
        ebeln LIKE ekko-ebeln,  "Nº do documento de compras
        bukrs LIKE ekko-bukrs,  "Empresa
        bstyp LIKE ekko-bstyp,  "Categoria do documento de compras
        bsart LIKE ekko-bsart,  "Tipo de documento de compras
        bedat LIKE ekko-bedat,  "Data do documento de compra
        matnr LIKE mara-matnr,  "Nº do material
        maktg LIKE makt-maktg,  "Texto breve de material em letras maiúsculas p/matchcodes
        ebelp LIKE ekpo-ebelp,  "Nº item do documento de compra
        reswk LIKE ekko-reswk,  "Centro de Saida em caso de transferência
        werks LIKE ekpo-werks,  "Centro de Destino em caso de transferência
        meins LIKE ekpo-meins,  "Unidade de medida do pedido
        butxt LIKE t001-butxt,  "Denominação da firma ou empresa
        name1 LIKE t001w-name1, "Nome Centro
      END OF wa_ekko.

DATA: BEGIN OF wa_romaneio,
        mark          TYPE c LENGTH 1,
        ch_referencia LIKE zsdt0001-ch_referencia,
        nr_romaneio   LIKE zsdt0001-nr_romaneio,
        dt_movimento  LIKE zsdt0001-dt_movimento,
        nr_safra      LIKE zsdt0001-nr_safra,
        id_cli_dest   LIKE zsdt0001-id_cli_dest,
        name1         LIKE kna1-name1,
        tp_frete      LIKE zsdt0001-tp_frete,
        peso_liq      LIKE zsdt0001-peso_liq,
        placa_cav     LIKE zsdt0001-placa_cav,
        nr_ticket     LIKE zsdt0001-nr_ticket,
        remessa       TYPE vbeln_vl,
        lgort         TYPE lgort_d,
      END OF wa_romaneio.

DATA: BEGIN OF wa_log_erros,
        type          TYPE bapi_mtype,
        number        TYPE symsgno,
        ch_referencia LIKE zsdt0001-ch_referencia,
        mensagem      TYPE bapi_msg,
      END OF wa_log_erros.

DATA: it_retorno           TYPE bapiret2 OCCURS 0 WITH HEADER LINE,
      it_itens             TYPE bapidlvreftosto OCCURS 0 WITH HEADER LINE,
      it_items             TYPE bapishpdelivnumb OCCURS 0 WITH HEADER LINE,
      v_deliv_numb         TYPE bapishpdelivnumb-deliv_numb,
      v_xblnr              TYPE vbrk-xblnr,
      wa_items             TYPE bapishpdelivnumb,
      it_ekko              LIKE STANDARD TABLE OF wa_ekko,
      it_romaneio          LIKE STANDARD TABLE OF wa_romaneio,
      it_roma_sel          LIKE STANDARD TABLE OF wa_romaneio,
      it_log_erros         LIKE STANDARD TABLE OF wa_log_erros,
      it_log_warni         LIKE STANDARD TABLE OF wa_log_erros,
      wa_roma_sel          LIKE wa_romaneio,
      wa_roma_pro          LIKE wa_romaneio,
      it_roma_log          LIKE STANDARD TABLE OF wa_romaneio,
      wa_roma_log          LIKE wa_romaneio,
      vg_lista             TYPE c LENGTH 1,
      vg_romaneio          TYPE c LENGTH 1,
      msg                  TYPE string,
      v_zsdt0001           TYPE zsdt0001,
      vl_matdocumentyear_r TYPE bapi2017_gm_head_ret-doc_year,
      vl_mat_doc_r         TYPE bapi2017_gm_head_ret-mat_doc,
      sl_vbkok_wa          TYPE vbkok,
      sl_vbpok             TYPE vbpok,
      tl_vbpok             TYPE TABLE OF vbpok,
      tl_prot              TYPE TABLE OF prott,
      sl_prot              TYPE prott,
      sl_zmmt0074          TYPE zmmt0074,
      wa_setleaf           TYPE setleaf,
      it_setleaf           LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE,
      v_nr_romaneio        TYPE zsdt0001-ch_referencia,
      vl_delivery_c        TYPE bapishpdelivnumb-deliv_numb.

DATA: sl_hdata    TYPE bapiobdlvhdrchg,
      sl_hcont    TYPE bapiobdlvhdrctrlchg,
      vl_delivery TYPE bapiobdlvhdrchg-deliv_numb,
      tl_bapiret2 TYPE bapiret2_t.

DATA: wa_header_data    TYPE bapiobdlvhdrchg,
      wa_header_control TYPE bapiobdlvhdrctrlchg,
      header_partner    TYPE TABLE OF bapidlvpartnerchg INITIAL SIZE 0 WITH HEADER LINE,
      item_data         TYPE TABLE OF bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
      item_control      TYPE TABLE OF bapiobdlvitemctrlchg INITIAL SIZE 0 WITH HEADER LINE,
      it_retorno2       TYPE TABLE OF bapiret2 INITIAL SIZE 0 WITH HEADER LINE,
      item_data_spl     TYPE TABLE OF /spe/bapiobdlvitemchg INITIAL SIZE 0 WITH HEADER LINE,
      nlinhas           TYPE i,
      tl_lines          TYPE TABLE OF tline,
      st_header         TYPE thead,
      st_lines          TYPE tline.
*&---------------------------------------------------------------------*
*&      Variáveis de Saída de Material
*&---------------------------------------------------------------------*
DATA: v_rfmng TYPE vbfa-rfmng,
      v_user  TYPE tvarv_val.

*-#133089-21.02.2024-JT-inicio
DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim

*&---------------------------------------------------------------------*
*&      SELECTION-SCREEN
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  SELECT-OPTIONS: p_ebeln  FOR ekko-ebeln,
                  p_bukrs  FOR ekko-bukrs,
                  p_centro FOR ekko-reswk,
                  p_destin FOR ekpo-werks,
                  p_matnr  FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETER      p_lifnr LIKE lfa1-lifnr OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

*-#133089-21.02.2024-JT-inicio
PARAMETERS: p_fataut TYPE char01 NO-DISPLAY.
*-#133089-21.02.2024-JT-fim

*-#144450-28.06.2024-JT-inicio
PARAMETERS: p_cockp  TYPE char02 NO-DISPLAY.
*-#144450-28.02.2024-JT-fim

*&---------------------------------------------------------------------*
*&      INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.

  IF NOT vg_lista IS INITIAL.
    PERFORM monta_lista_ped_compra.
  ENDIF.

  IF NOT vg_romaneio IS INITIAL.
    PERFORM monta_lista_romaneios.
  ENDIF.

*&---------------------------------------------------------------------*
*&      START-OF-SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  CLEAR: vl_delivery_c, v_nr_romaneio.

*-#133089-21.02.2024-JT-inicio
*-----------------------------------------------
*-verifica se é faturamento automatico
*-----------------------------------------------
  IF p_fataut = abap_true.
    vg_faturamento_autom = p_fataut.
    CREATE OBJECT lc_faturamento_automatico.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  IF sy-tcode = 'ZLES0106' OR sy-tcode = 'ZLES0115' OR sy-tcode = 'ZMM0127' OR sy-tcode = 'ZLES0136' OR
    vg_faturamento_autom = abap_true.
    GET PARAMETER ID 'Z_MY_PARAMETER_1' FIELD v_nr_romaneio.
    SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery_c.
  ENDIF.

* Verifica Usuário
  PERFORM z_seleciona_tvarvc.

  SELECT SINGLE lifnr INTO lfa1-lifnr
                      FROM lfa1
                     WHERE lifnr EQ p_lifnr.

  IF NOT sy-subrc IS INITIAL.
*-#133089-21.02.2024-JT-inicio
    CASE vg_faturamento_autom.
      WHEN abap_off.
        MESSAGE 'Agente de frete não cadastrado.' TYPE 'I'.
      WHEN abap_true.
        DATA(l_mesg) = 'Agente de frete não cadastrado.'.
        lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = v_nr_romaneio i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
    ENDCASE.
*-#133089-21.02.2024-JT-fim
  ELSE.
    CLEAR: vg_lista, vg_romaneio, it_retorno, it_itens.
    PERFORM seleciona_pedido_compra.
    IF v_nr_romaneio IS NOT INITIAL. " Chamada pela zles0106 (somente um pedido por vez)
      IF it_ekko[] IS NOT INITIAL.
        READ TABLE it_ekko INTO wa_ekko INDEX 1.
        REFRESH: it_romaneio, it_roma_sel.
        PERFORM listar_romaneios. " testa bloqueio ou lista
        IF it_romaneio[] IS NOT INITIAL.
          READ TABLE  it_romaneio INTO wa_romaneio INDEX 1.
          PERFORM acumula_romaneios USING wa_romaneio-ch_referencia.
          IF NOT it_roma_sel IS INITIAL.
            PERFORM gera_delivery.
*-#155161-21.10.2024-JT-inicio
            READ TABLE it_log_erros INTO wa_log_erros
              WITH KEY ch_referencia = wa_romaneio-ch_referencia
                          type          = 'E'.
            IF sy-subrc = 0.
              wa_roma_log-ch_referencia = wa_romaneio-ch_referencia.
              wa_roma_log-nr_romaneio   = wa_romaneio-nr_romaneio.
              PERFORM mostra_log_erro.
            ENDIF.
*-#155161-21.10.2024-JT-fim
          ELSE.
*** BUG 43008 - csb - inicio
            READ TABLE it_log_erros INTO wa_log_erros
              WITH KEY ch_referencia = wa_romaneio-ch_referencia
                          type          = 'E'.
            IF sy-subrc = 0.
              wa_roma_log-ch_referencia = wa_romaneio-ch_referencia.
              wa_roma_log-nr_romaneio   = wa_romaneio-nr_romaneio.
              PERFORM mostra_log_erro.
            ENDIF.
          ENDIF.
*** BUG 43008 - csb - fim
        ENDIF.
      ENDIF.
    ELSE.
      PERFORM monta_lista_ped_compra.
      vg_lista = 'X'.
    ENDIF.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_PEDIDO_COMPRA
*&---------------------------------------------------------------------*
*       Seleciona registros de pedido de compra
*----------------------------------------------------------------------*
FORM seleciona_pedido_compra .

  DATA: v_spras TYPE spras.

  CLEAR: it_ekko.

  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'
    EXPORTING
      input  = 'PT'
    IMPORTING
      output = v_spras.

  SELECT ek~ebeln ek~bukrs ek~bstyp ek~bsart ek~bedat
         po~matnr mt~maktg po~ebelp ek~reswk po~meins
         em~butxt fi~name1 po~werks
    INTO CORRESPONDING FIELDS OF TABLE it_ekko
    FROM ekko AS ek
    INNER JOIN ekpo  AS po ON po~ebeln EQ ek~ebeln
     LEFT JOIN makt  AS mt ON mt~matnr EQ po~matnr AND mt~spras EQ v_spras
     LEFT JOIN t001  AS em ON em~bukrs EQ ek~bukrs AND em~land1 EQ 'BR '
     LEFT JOIN t001w AS fi ON fi~werks EQ ek~reswk AND fi~land1 EQ 'BR '
   WHERE ek~bukrs IN p_bukrs
     AND ek~ebeln IN p_ebeln
     AND ek~bstyp EQ 'F'
     AND ek~bsart EQ 'ZUB '
     AND po~werks IN p_destin
     AND ek~reswk IN p_centro
     AND po~matnr IN p_matnr
     AND po~loekz EQ ' '
     AND po~elikz EQ ' '
   ORDER BY ek~ebeln po~ebelp.

ENDFORM.                    " SELECIONA_PEDIDO_COMPRA

*&---------------------------------------------------------------------*
*&      Form  MONTA_LISTA_PED_COMPRA
*&---------------------------------------------------------------------*
*       Gera lista de pedidos de compra
*----------------------------------------------------------------------*
FORM monta_lista_ped_compra .

  SET TITLEBAR 'LISTACOMPRA'.

  NEW-PAGE LINE-SIZE 164
           LINE-COUNT 40.

  LOOP AT it_ekko INTO wa_ekko.

    PERFORM imprime_ekko USING wa_ekko ' '.

    HIDE: wa_ekko-ebeln, wa_ekko-bukrs, wa_ekko-butxt, wa_ekko-bedat,
          wa_ekko-matnr, wa_ekko-maktg, wa_ekko-ebelp, wa_ekko-reswk,
          wa_ekko-meins, wa_ekko-name1, wa_ekko-werks.

  ENDLOOP.

ENDFORM.                    " MONTA_LISTA_PED_COMPRA

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_EKKO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprime_ekko USING p_wa_ekko LIKE wa_ekko p TYPE c.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  IF p NE 'X'.
    WRITE /001 p_wa_ekko-ebeln HOTSPOT COLOR = 4.
  ELSE.
    WRITE: /001 p_wa_ekko-ebeln.
  ENDIF.

  WRITE:  012 p_wa_ekko-bukrs,
          018 p_wa_ekko-butxt,
          045 p_wa_ekko-bedat,
          057 p_wa_ekko-matnr,
          077 p_wa_ekko-maktg,
          119 p_wa_ekko-reswk,
          125 p_wa_ekko-name1,
          157 p_wa_ekko-werks,
          163 p_wa_ekko-meins.

ENDFORM.                    " IMPRIME_EKKO

*&---------------------------------------------------------------------*
*&      TOP-OF-PAGE
*&---------------------------------------------------------------------*
TOP-OF-PAGE.

  PERFORM imprime_ekko_cab.
  ULINE.

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_EKKO_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprime_ekko_cab .

  ULINE.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.

  WRITE:/001 'Nº Pedido',
         012 'Empresa',
         045 'Dt.Pedido',
         057 'Material',
         119 'Centro Origem',
         157 'Dest.',
         163 'Un.'.

ENDFORM.                    " IMPRIME_EKKO_CAB

*&---------------------------------------------------------------------*
*&      Form  LISTAR_ROMANEIOS
*&---------------------------------------------------------------------*
*       Lista de Romaneios Livres
*----------------------------------------------------------------------*
FORM listar_romaneios .

  DATA: BEGIN OF wa_enq.
          INCLUDE STRUCTURE seqg7.
  DATA: END OF wa_enq.

  DATA: garg LIKE seqg3-garg,
        enq  LIKE STANDARD TABLE OF wa_enq.

  READ CURRENT LINE.

  CALL FUNCTION 'ZENQUEUE_ROMANEIOS_CENTRO'
    EXPORTING
      v_mode         = 'X'
      v_mandt        = sy-mandt
      v_werks        = wa_ekko-reswk
      v_matnr        = wa_ekko-matnr
      v_scope        = '2'
      v_wait         = ' '
      v_collect      = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  CASE sy-subrc.
    WHEN 1.

      CONCATENATE sy-mandt wa_ekko-reswk wa_ekko-matnr INTO garg.

      CALL FUNCTION 'ENQUE_READ2'
        EXPORTING
          gname  = 'ZSDT0001'
          garg   = garg
          guname = '*'
        TABLES
          enq    = enq.

      READ TABLE enq INTO wa_enq WITH KEY gname = 'ZSDT0001'.

      CONCATENATE 'Romaneios do centro' wa_ekko-reswk 'material' wa_ekko-matnr 'bloqueado com o usuário' wa_enq-guname INTO msg SEPARATED BY space.

      MESSAGE msg TYPE 'E'.

    WHEN 2 OR 3.

      MESSAGE 'Erro em bloqueio!' TYPE 'E'.

    WHEN OTHERS.
      CLEAR: it_retorno, it_itens.
      PERFORM seleciona_romaneios USING wa_ekko.
      IF v_nr_romaneio IS INITIAL.
        IF NOT it_romaneio IS INITIAL.
          vg_romaneio = 'X'.
          SET PF-STATUS 'INICIAL'.
          PERFORM monta_lista_romaneios.
          CLEAR vg_lista.
        ELSE.
          CONCATENATE 'Não existe romaneios livres para o centro/material:' wa_ekko-reswk wa_ekko-matnr INTO msg SEPARATED BY space.
          MESSAGE msg TYPE 'I'.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " LISTAR_ROMANEIOS

*&---------------------------------------------------------------------*
*&      AT LINE-SELECTION
*&---------------------------------------------------------------------*
AT LINE-SELECTION.

  DATA: vg_field TYPE string,
        vg_value TYPE string.

  IF vg_lista IS INITIAL.
    CLEAR vg_romaneio.
  ENDIF.

  IF vg_romaneio IS INITIAL.

    READ CURRENT LINE.
    GET CURSOR FIELD vg_field VALUE vg_value.

    IF vg_field EQ 'WA_EKKO-EBELN'.
      PERFORM listar_romaneios.
    ENDIF.

  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  SELECIONA_ROMANEIOS
*&---------------------------------------------------------------------*
*       Selecionar romaneios do tipo de saída do mesmo centro
*       que não tenha remessa
*----------------------------------------------------------------------*
FORM seleciona_romaneios USING p_ekko LIKE wa_ekko.

  RANGES: lra_branch FOR zsdt0001-branch.

  CLEAR: lra_branch[].

  IF p_ekko-reswk IS INITIAL.
    CLEAR: it_romaneio[].
    EXIT.
  ENDIF.

  lra_branch-sign   = 'I'.
  lra_branch-option = 'EQ'.
  lra_branch-low    = p_ekko-reswk.
  APPEND lra_branch.

  SELECT SINGLE *
    FROM zsdt_depara_cen INTO @DATA(lwa_zsdt_depara_cen)
   WHERE centrov_1 EQ @p_ekko-reswk.

  IF ( sy-subrc EQ 0 ) AND ( lwa_zsdt_depara_cen-centro_real NE p_ekko-reswk  ) .
    lra_branch-low = lwa_zsdt_depara_cen-centro_real.
    APPEND lra_branch.
  ENDIF.

  SELECT zr~ch_referencia
         zr~nr_romaneio
         zr~dt_movimento
         zr~nr_safra
         zr~id_cli_dest
         cl~name1
         zr~tp_frete
         zr~peso_liq
         zr~placa_cav
         zr~nr_ticket
    INTO CORRESPONDING FIELDS OF TABLE it_romaneio
    FROM zsdt0001 AS zr
    INNER JOIN kna1 AS cl ON cl~kunnr EQ zr~id_cli_dest
   WHERE zr~branch  IN lra_branch
     AND zr~matnr   EQ p_ekko-matnr
     AND zr~tp_movimento EQ 'S'
     AND cl~land1   EQ 'BR '
     AND zr~status  EQ ' '
     AND zr~doc_rem EQ ''
     AND ( ( zr~vbeln   EQ '' ) OR ( zr~vbeln IN p_ebeln AND zr~vbeln NE '' ) ).

*-#144450-28.06.2024-JT-inicio
  IF p_cockp = '04'.  "Fertilizantes
    SELECT zr~ch_referencia
           zr~nr_romaneio
           zr~dt_movimento
           zr~nr_safra
           zr~id_cli_dest
           cl~name1
           zr~tp_frete
           zr~peso_liq
           zr~placa_cav
           zr~nr_ticket
      APPENDING CORRESPONDING FIELDS OF TABLE it_romaneio
      FROM zsdt0001 AS zr
     INNER JOIN kna1 AS cl  ON cl~kunnr EQ zr~id_cli_dest
     WHERE zr~branch        IN lra_branch
       AND zr~matnr         EQ p_ekko-matnr
       AND zr~tp_movimento  EQ 'S'
       AND cl~land1         EQ 'BR '
       AND zr~status        EQ 'X'
       AND zr~st_proc       EQ '12'
       AND zr~doc_rem       EQ ''
       AND ( ( zr~vbeln     EQ '' ) OR ( zr~vbeln IN p_ebeln AND zr~vbeln NE '' ) ).
  ENDIF.
*-#144450-28.06.2024-JT-fim

  IF v_nr_romaneio IS NOT INITIAL.
    DELETE it_romaneio WHERE ch_referencia NE v_nr_romaneio.
  ENDIF.

ENDFORM.                    " SELECIONA_ROMANEIOS

*&---------------------------------------------------------------------*
*&      Form  MONTA_LISTA_ROMANEIOS
*&---------------------------------------------------------------------*
*       Gera lista de romaneios livres
*----------------------------------------------------------------------*
FORM monta_lista_romaneios .

  SET TITLEBAR 'LISTAROMA'.

  NEW-PAGE LINE-SIZE 164
           LINE-COUNT 40.

  PERFORM imprime_roma_cab.

  LOOP AT it_romaneio INTO wa_romaneio.

    PERFORM imprime_roma USING wa_romaneio.

    HIDE: wa_romaneio-mark,
          wa_romaneio-ch_referencia,
          wa_romaneio-nr_romaneio,
          wa_romaneio-dt_movimento,
          wa_romaneio-nr_safra,
          wa_romaneio-id_cli_dest,
          wa_romaneio-name1,
          wa_romaneio-tp_frete,
          wa_romaneio-peso_liq,
          wa_romaneio-placa_cav,
          wa_romaneio-nr_ticket.

  ENDLOOP.

ENDFORM.                    " MONTA_LISTA_ROMANEIOS

*&---------------------------------------------------------------------*
*&      AT USER-COMMAND
*&---------------------------------------------------------------------*
AT USER-COMMAND.

  CASE sy-ucomm.
    WHEN 'BACKG' OR 'RWG'.
      PERFORM desbloqueia_dados.
    WHEN 'LISTAR'.
      PERFORM listar_romaneios.
    WHEN 'LOG_ERR'.
      PERFORM mostra_log_erro.
    WHEN 'REMESSA'.
      READ CURRENT LINE.
      IF NOT wa_roma_log-remessa IS INITIAL.
        SET PARAMETER ID 'VL' FIELD wa_roma_log-remessa.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ELSE.
        PERFORM mostra_log_erro.
      ENDIF.

    WHEN 'EXEC'.

      CLEAR: it_roma_sel, it_log_erros, it_log_warni.

      DO.
        READ LINE sy-index FIELD VALUE wa_romaneio-mark wa_romaneio-ch_referencia.
        IF sy-subrc <> 0.
          EXIT.
        ELSE.
          IF wa_romaneio-mark EQ 'X'.
            PERFORM acumula_romaneios USING wa_romaneio-ch_referencia.
          ENDIF.
        ENDIF.
      ENDDO.

      IF NOT it_roma_sel IS INITIAL.
        PERFORM gera_delivery.
      ELSE.
*** BUG 43008 - CSB ini
        READ TABLE it_log_erros INTO wa_log_erros
        WITH KEY ch_referencia = wa_romaneio-ch_referencia
                    type          = 'E'.
        IF sy-subrc = 0.
          wa_roma_log-ch_referencia = wa_romaneio-ch_referencia.
          wa_roma_log-nr_romaneio   = wa_romaneio-nr_romaneio.
          PERFORM mostra_log_erro.
        ENDIF.
      ENDIF.
*** BUG 43008 - CSB fim
    WHEN 'TODAS'.

      DO.
        READ LINE sy-index.
        IF sy-subrc <> 0.
          EXIT.
        ELSE.
          MODIFY LINE sy-index FIELD VALUE wa_romaneio-mark FROM 'X'.
        ENDIF.
      ENDDO.

    WHEN 'DESMARC'.

      DO.
        READ LINE sy-index.
        IF sy-subrc <> 0.
          EXIT.
        ELSE.
          MODIFY LINE sy-index FIELD VALUE wa_romaneio-mark FROM ' '.
        ENDIF.
      ENDDO.

    WHEN 'INVERT'.

      DO.
        READ LINE sy-index FIELD VALUE wa_romaneio-mark.
        IF sy-subrc <> 0.
          EXIT.
        ELSE.
          IF wa_romaneio-mark NE 'X'.
            MODIFY LINE sy-index FIELD VALUE wa_romaneio-mark FROM 'X'.
          ELSE.
            MODIFY LINE sy-index FIELD VALUE wa_romaneio-mark FROM ' '.
          ENDIF.
        ENDIF.
      ENDDO.

  ENDCASE.

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_ROMA_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprime_roma_cab .

  PERFORM imprime_ekko_cab.
  PERFORM imprime_ekko USING wa_ekko 'X'.

  ULINE.

  FORMAT COLOR COL_GROUP INTENSIFIED ON.
  WRITE: /003 'Romaneio',
          014 'Dt. Mov.',
          026 'Safra',
          032 'Cliente',
          081 'Tp. Frete',
          091 '    Peso Líquido',
          114 'Placa',
          123 'Ticket     '.

  ULINE.

ENDFORM.                    " IMPRIME_ROMA_CAB

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_ROMA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprime_roma  USING    p_wa_romaneio LIKE wa_romaneio.

  FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

  WRITE: /001 p_wa_romaneio-mark AS CHECKBOX,
          003 p_wa_romaneio-nr_romaneio,
          014 p_wa_romaneio-dt_movimento,
          026 p_wa_romaneio-nr_safra,
          032 p_wa_romaneio-id_cli_dest,
          044 p_wa_romaneio-name1,
          081 p_wa_romaneio-tp_frete,
          091 p_wa_romaneio-peso_liq,
          114 p_wa_romaneio-placa_cav,
          123 p_wa_romaneio-nr_ticket.

ENDFORM.                    " IMPRIME_ROMA

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_ROMA_CAB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprime_roma_cab_detalhe .

  PERFORM imprime_ekko_cab.
  PERFORM imprime_ekko USING wa_ekko 'X'.

  ULINE.

  FORMAT COLOR COL_GROUP INTENSIFIED ON.
  WRITE: /001 'Romaneio',
          011 'Dt. Mov.',
          023 'Safra',
          029 'Cliente',
          078 'Tp. Frete',
          088 '    Peso Líquido',
          111 'Placa',
          120 'Ticket     ',
          132 'Remessa',
          144 'Depósito',
          156 'Status   '.

  ULINE.

ENDFORM.                    " IMPRIME_ROMA_CAB

*&---------------------------------------------------------------------*
*&      Form  IMPRIME_ROMA_DETALHES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprime_roma_detalhes  USING    p_wa_romaneio LIKE wa_romaneio.

  IF p_wa_romaneio-mark EQ 'X'.
    READ TABLE it_log_warni INTO wa_log_erros
          WITH KEY ch_referencia = p_wa_romaneio-ch_referencia
                   type          = 'E'.
    IF sy-subrc NE 0.
      FORMAT COLOR COL_NORMAL INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
    ENDIF.
  ELSE.
    FORMAT COLOR COL_TOTAL INTENSIFIED OFF.
  ENDIF.

  WRITE: /001 p_wa_romaneio-nr_romaneio,
          011 p_wa_romaneio-dt_movimento,
          023 p_wa_romaneio-nr_safra,
          029 p_wa_romaneio-id_cli_dest,
          041 p_wa_romaneio-name1,
          078 p_wa_romaneio-tp_frete,
          088 p_wa_romaneio-peso_liq,
          111 p_wa_romaneio-placa_cav,
          120 p_wa_romaneio-nr_ticket,
          132 p_wa_romaneio-remessa,
          144 p_wa_romaneio-lgort.

  IF p_wa_romaneio-mark EQ 'X'.
    READ TABLE it_log_warni INTO wa_log_erros
          WITH KEY ch_referencia = p_wa_romaneio-ch_referencia
                   type          = 'E'.
    IF sy-subrc NE 0.
      WRITE 156 icon_green_light  AS ICON.
    ELSE.
      WRITE 156 icon_yellow_light AS ICON.
    ENDIF.
  ELSE.
    WRITE 156 icon_red_light AS ICON.
  ENDIF.

ENDFORM.                    " IMPRIME_ROMA_DETALHES

*&---------------------------------------------------------------------*
*&      Form  ACUMULA_ROMANEIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ROMANEIO_CH_REFERENCIA  text
*----------------------------------------------------------------------*
FORM acumula_romaneios  USING  p_wa_romaneio_ch_referencia.

  CLEAR: wa_roma_sel.

  READ TABLE it_romaneio INTO wa_roma_sel WITH KEY ch_referencia = p_wa_romaneio_ch_referencia.

  wa_log_erros-type          = 'E'.
  wa_log_erros-number        = '000'.
  wa_log_erros-ch_referencia = p_wa_romaneio_ch_referencia.

*  IF SY-SUBRC EQ 0.

*    SELECT SINGLE VALFROM
*      INTO WA_ROMA_SEL-LGORT
*      FROM SETLEAF
*     WHERE SETNAME EQ 'DEPOSITO_TRANSF'.
*
*    IF SY-SUBRC EQ 0.
*      IF WA_ROMA_SEL-LGORT IS INITIAL.
*        WA_LOG_ERROS-MENSAGEM = 'Parâmetro de Depósito (DEPOSITO_TRANSF) está vazio'.
*        APPEND WA_LOG_ERROS TO IT_LOG_ERROS.
*      ENDIF.
*    ELSE.
*      WA_LOG_ERROS-MENSAGEM = 'Parâmetro de Depósito (DEPOSITO_TRANSF) não localizado'.
*      APPEND WA_LOG_ERROS TO IT_LOG_ERROS.
*    ENDIF.

*  ENDIF.

  " Verifica Picking - ( Camila Brand )

  DATA: tg_0023         TYPE TABLE OF zmm0023,
        wa_0023         TYPE zmm0023,
        wa_0023_aux     TYPE sy-subrc, "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
        "VL_CENTRO_A     TYPE WERKS_D,
        vl_clabs_f      TYPE labst,
        vl_clabs_e      TYPE labst,
        vl_clabs_a      TYPE labst,
        vl_total        TYPE labst,
        vl_aux          TYPE char18,
        vl_msn1         TYPE char50,
        vl_msn2         TYPE char50,
        v_safra_a       TYPE zsdt0001-nr_safra,
        "WA_TL_ZSDT0001  TYPE ZSDT0001,
        "IT_ZSDT0001     TYPE TABLE OF ZSDT0001,
        wa_setleaf      TYPE setleaf,
        vmatnr          TYPE matnr18,
        it_setleaf      LIKE TABLE OF wa_setleaf INITIAL SIZE 0 WITH HEADER LINE,
        sl_zsdt0001_ent TYPE zsdt0001.


  SELECT SINGLE * INTO @DATA(wa_zsdt0001)
   FROM zsdt0001
   WHERE ch_referencia EQ @p_wa_romaneio_ch_referencia.

  IF sy-subrc IS NOT INITIAL.
    wa_log_erros-mensagem = 'Não encontrado romaneio com chave' && space && p_wa_romaneio_ch_referencia && '!'.
    APPEND wa_log_erros TO it_log_erros.
    EXIT.
  ENDIF.
  "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*  SELECT SINGLE * INTO @DATA(wa_zmmt0017)
*    FROM zmmt0017
*   WHERE matnr       EQ @wa_zsdt0001-matnr
*     AND centro_fixo EQ @wa_zsdt0001-branch.

  zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
       EXPORTING
         i_material        = wa_zsdt0001-matnr
         i_centro_fixo     = wa_zsdt0001-branch
       IMPORTING
         e_single_depara   = DATA(wa_zmmt0017)
     ).

  "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 -  FIM
  IF wa_zmmt0017 IS NOT INITIAL.
    TRY.

        zcl_deposito=>zif_deposito~get_instance(
          )->get_deposito_material_filial(
          EXPORTING
            i_matnr          = wa_zsdt0001-matnr
            i_tp_produto     = CONV #( COND string( WHEN wa_zsdt0001-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )
            i_bukrs          = wa_zsdt0001-bukrs
            i_branch         = wa_zsdt0001-branch   " Local de negócios
            i_eudr           = wa_zsdt0001-eudr   "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940
          IMPORTING
            e_lgort          = DATA(e_lgort)           " Depósito
            e_centro_a_fixar = DATA(e_centro_a_fixar)  " Centro
        ).

        wa_roma_sel-lgort = e_lgort.
        APPEND wa_roma_sel TO it_roma_sel.

      CATCH zcx_deposito INTO DATA(ex_deposito).    " .
        wa_log_erros-mensagem = ex_deposito->zif_error~get_msg_erro( ).
        APPEND wa_log_erros TO it_log_erros.
        EXIT.
    ENDTRY.
  ELSE.
    APPEND wa_roma_sel TO it_roma_sel.
  ENDIF.

  SELECT * FROM zmm0023 INTO TABLE tg_0023.
*  SORT tg_0023 BY  werks ASCENDING matnr ASCENDING cwerks DESCENDING.
  SORT tg_0023 BY  werks ASCENDING matnr ASCENDING matkl ASCENDING cwerks DESCENDING. "PBALVES

  CLEAR wa_0023.
  READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = wa_zsdt0001-branch "COMENTADO PAR SUBIR REQUEST 25.01.24
                                           matnr = wa_zsdt0001-matnr. "lê o primeiro - SMC CS2023000120 Urgente - Atualização tela de bloqueio

  "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria PSA
  IF sy-subrc NE 0.
    SELECT SINGLE matkl INTO @DATA(_matkl) FROM mara WHERE matnr = @wa_zsdt0001-matnr.
    READ TABLE tg_0023 INTO wa_0023 WITH KEY werks = wa_zsdt0001-branch
                                             matkl = _matkl.
  ENDIF.

  "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
  CLEAR wa_0023_aux.
  IF
    sy-subrc NE 0.
    wa_0023_aux = sy-subrc.
  ENDIF.
  "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

  "Centro do Pedido Virtual, não faz conferencia de estoque
  IF ( NOT sy-subrc IS INITIAL AND wa_0023-status NE 'A' )  AND ( wa_zsdt0001-matnr IS NOT INITIAL ) AND ( wa_ekko-reswk = wa_zsdt0001-branch ).

*Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - INICIO
*    SELECT SINGLE * INTO WA_ZMMT0017
*      FROM ZMMT0017
*     WHERE MATNR       EQ WA_ZSDT0001-MATNR
*       AND CENTRO_FIXO EQ WA_ZSDT0001-BRANCH.

    zcl_depara_centro_fixo_afixar=>zif_depara_centro_fixo_afixar~get_dados_depara(
             EXPORTING
               i_material        = wa_zsdt0001-matnr
               i_centro_fixo     = wa_zsdt0001-branch
             IMPORTING
               e_single_depara   = wa_zmmt0017
           ).

    IF wa_zmmt0017 IS NOT INITIAL.
      "Parâmetros Centro Fixo x Centro Afixar EUDR - BG #152940 - FIM

      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC
      IF wa_0023_aux IS NOT INITIAL.
        MESSAGE e897(sd) WITH  'Falta parâmetros na ZMM0029. '
                                  'Favor entrar em contato com '
                                   'a área de controladoria e estoque. '.
      ENDIF.
      "141033 CS2023000120 - ZMM0029 Controle por Grupo de Mercadoria SMC

      SELECT COUNT(*)
       FROM tvarvc
        WHERE name = 'MAGGI_BIODIESEL'
        AND   low  =   wa_zsdt0001-matnr.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.
      SELECT SINGLE clabs
          FROM mchb
          INTO vl_clabs_f
        WHERE  matnr EQ wa_zsdt0001-matnr
          AND  werks EQ wa_zsdt0001-branch
          AND  lgort EQ e_lgort
          AND  charg EQ wa_zsdt0001-nr_safra.

      IF NOT e_centro_a_fixar IS INITIAL.
        IF wa_zsdt0001-nr_safra GT '2019'.
          CONCATENATE wa_zsdt0001-nr_safra '_' wa_zsdt0001-branch INTO v_safra_a.
        ELSE.
          v_safra_a = wa_zsdt0001-nr_safra.
        ENDIF.
        SELECT SINGLE clabs
          FROM mchb
          INTO vl_clabs_a
        WHERE  matnr EQ wa_zsdt0001-matnr
          AND  werks EQ e_centro_a_fixar
          AND  lgort EQ e_lgort
          AND  charg EQ v_safra_a.
      ENDIF.

      "Checa se vai ter entrada de residuo
      "ALRS C
      CLEAR vl_clabs_e.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zsdt0001-matnr
        IMPORTING
          output = vmatnr.

      "ALRS
      wa_zsdt0001-matnr = vmatnr.

      SELECT * INTO TABLE it_setleaf
        FROM setleaf
       WHERE setname EQ 'RESIDUO'
         AND valfrom EQ wa_zsdt0001-matnr.
      "
      IF sy-subrc = 0.

        SELECT SINGLE *
          FROM zmmt0074
          INTO sl_zmmt0074
         WHERE werks EQ wa_zsdt0001-branch
           AND matnr EQ wa_zsdt0001-matnr.

        IF sl_zmmt0074-entrada_rom = 'S'. "Checa romaneio de entrada
          IF wa_zsdt0001-id_referencia IS NOT INITIAL.
            DATA(_nr_romaneio) = wa_zsdt0001-id_referencia.

            CLEAR: sl_zsdt0001_ent.

            SELECT SINGLE *
              FROM zsdt0001
              INTO sl_zsdt0001_ent
             WHERE bukrs         EQ wa_zsdt0001-bukrs
               AND branch        EQ wa_zsdt0001-branch
               AND tp_movimento  EQ 'E'
               AND nr_romaneio   EQ _nr_romaneio
               AND nr_safra      EQ wa_zsdt0001-nr_safra.

            IF sl_zsdt0001_ent-ch_refer_ent IS NOT INITIAL.

              DATA(_peso_orig) = sl_zsdt0001_ent-peso_liq. "CS2017001903

              CLEAR sl_zsdt0001_ent-peso_liq.
              SELECT SUM( peso_liq )
                FROM zsdt0001
                INTO sl_zsdt0001_ent-peso_liq
               WHERE bukrs         EQ sl_zsdt0001_ent-bukrs
                 AND branch        EQ sl_zsdt0001_ent-branch
                 AND tp_movimento  EQ 'E'
                 AND ch_refer_ent  EQ sl_zsdt0001_ent-ch_refer_ent
                 AND nr_safra      EQ sl_zsdt0001_ent-nr_safra.

              IF _peso_orig = sl_zsdt0001_ent-peso_liq.
                wa_log_erros-mensagem = 'Não foram encontrados todos os romaneios de desmembramento!'.
                APPEND wa_log_erros TO it_log_erros.
              ENDIF.

            ENDIF.

            IF sy-subrc =  0.
              IF  wa_zsdt0001-peso_liq GT sl_zsdt0001_ent-peso_liq.
                vl_clabs_e =  wa_zsdt0001-peso_liq - sl_zsdt0001_ent-peso_liq.
              ELSE.
                "Não gera  doc. entrada
              ENDIF.

            ELSE.
              wa_log_erros-mensagem = 'Não foi encontrado o romaneio de entrada nesta data!'.
              APPEND wa_log_erros TO it_log_erros.
            ENDIF.
          ENDIF.
        ELSE.
          vl_clabs_e = wa_zsdt0001-peso_liq.
        ENDIF.
      ENDIF.

      vl_total = vl_clabs_a + vl_clabs_f + vl_clabs_e.

      IF wa_zsdt0001-peso_liq GT vl_total.
        vl_aux = vl_total.
        CONDENSE vl_aux NO-GAPS.
        CONCATENATE 'O total' vl_aux 'do centro' wa_zsdt0001-branch 'e' e_centro_a_fixar 'é menor que a quantidade do picking' INTO wa_log_erros-mensagem SEPARATED BY space.
        APPEND wa_log_erros TO it_log_erros.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.                    " ACUMULA_ROMANEIOS

*&---------------------------------------------------------------------*
*&      Form  GERA_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gera_delivery.

  DATA: v_primeira_vez TYPE char1,
        v_erro         TYPE c LENGTH 1,
        v_editar       TYPE c,
        v_due_date     TYPE ledat,
        vl_route       TYPE route,
        vl_ablad       TYPE ekpv-ablad,
        vl_charg       TYPE eket-charg,
        vl_tknum       TYPE vttk-tknum,
        tl_return      TYPE bapiret2_t,
        sl_return      TYPE bapiret2,
        vl_kunnr       TYPE kna1-kunnr,
        vl_lifnr       TYPE lfa1-lifnr,
        vl_error_ent_r TYPE c,
        vmatnr         TYPE matnr18,
        msg_text       TYPE string,
        e_lgort	       TYPE lgort_d,
        lc_mesg        TYPE string,  "*-#155161-21.10.2024-JT-inicio
        lc_biodiesel   TYPE char01.  "*-#155161-21.10.2024-JT-inicio

  CLEAR: it_roma_log.


  LOOP AT it_roma_sel INTO wa_roma_pro.

    CLEAR: it_itens[], it_retorno,
           it_items[], v_deliv_numb, it_retorno, v_erro, vl_tknum, e_lgort, vl_ablad, vl_route.
    DATA(_biodiesel) = abap_false.

    READ TABLE it_log_erros INTO wa_log_erros WITH KEY ch_referencia = wa_roma_pro-ch_referencia.

    IF sy-subrc IS NOT INITIAL.

      SELECT SINGLE * FROM zsdt0001 INTO v_zsdt0001 WHERE ch_referencia EQ wa_roma_pro-ch_referencia.

      IF wa_ekko-bsart EQ 'ZUB'.
        SELECT COUNT(*)
        FROM tvarvc
         WHERE name = 'MAGGI_BIODIESEL'
         AND   low  =   v_zsdt0001-matnr.
        IF sy-subrc = 0.
          wa_roma_pro-peso_liq = v_zsdt0001-qtde_remessa.
          _biodiesel = abap_true.
        ENDIF.
      ENDIF.

*-#155161-21.10.2024-JT-inicio
*-----------------------------------
*---- Biodiesel: Movto estoque, preco Custos
*-----------------------------------
      TRY.
          lc_biodiesel = zcl_faturamento=>zif_faturamento~get_instance(
                                     )->set_transfere_biodiesel_frota( v_zsdt0001-ch_referencia
                                     ).
        CATCH zcx_error INTO DATA(lc_error).
          MESSAGE ID lc_error->msgid TYPE lc_error->msgty NUMBER lc_error->msgno WITH lc_error->msgv1 lc_error->msgv2 lc_error->msgv3 lc_error->msgv4
             INTO lc_mesg.
          wa_log_erros-mensagem = lc_mesg.
          APPEND wa_log_erros  TO it_log_erros.
          EXIT.
      ENDTRY.
*-#155161-21.10.2024-JT-fim

      "verifica se existe o itinerario
      SELECT SINGLE route ablad
        INTO ( vl_route, vl_ablad )
        FROM ekpv
        WHERE ebeln = wa_ekko-ebeln.


      IF sy-tcode NE 'ZLES0115'.
        IF vl_ablad IS NOT INITIAL.
          e_lgort = vl_ablad+0(4).
        ELSE.

          SELECT SINGLE * INTO @DATA(wa_zmmt0017)
            FROM zmmt0017
           WHERE matnr       EQ @v_zsdt0001-matnr
             AND centro_fixo EQ @v_zsdt0001-branch.

          IF sy-subrc EQ 0.
            TRY .
                zcl_deposito=>zif_deposito~get_instance(
                  )->get_deposito_material_filial(
                  EXPORTING
                    i_matnr          = v_zsdt0001-matnr
                    i_tp_produto     = CONV #( COND string( WHEN v_zsdt0001-tp_transgenia(1) EQ 'C' THEN zif_carga=>st_tp_transgeniase_co ELSE 'RR' ) )
                    i_bukrs          = v_zsdt0001-bukrs
                    i_branch         = v_zsdt0001-branch
                    i_eudr           = v_zsdt0001-eudr   "US 152850 11/10/2024 WPP ---->>>
                  IMPORTING
                    e_lgort          = e_lgort
                    e_centro_a_fixar = DATA(e_centro_a_fixar)
                ).
              CATCH zcx_deposito INTO DATA(ex_deposito).    " .
                ex_deposito->zif_error~published_erro( EXPORTING i_msgty = 'W' i_msgty_display = 'W' ).
                CONTINUE.
            ENDTRY.

          ELSE.
            "Bug Solto 146218
            SELECT SINGLE mtart INTO @DATA(_mtart) FROM mara WHERE matnr = @v_zsdt0001-matnr.
            "
            SELECT COUNT(*)
              FROM tvarvc
               WHERE name = 'MAGGI_BIODIESEL'
               AND   low  =   v_zsdt0001-matnr.
            IF sy-subrc = 0.
              IF _mtart = 'ZFER'.
                e_lgort = 'PR01'.
              ELSE.
                SELECT SINGLE lgort
                    FROM ekpo INTO e_lgort
                    WHERE ebeln EQ wa_ekko-ebeln.
              ENDIF.
            ELSE.
              SELECT SINGLE lgort
                FROM ekpo INTO e_lgort
               WHERE ebeln EQ wa_ekko-ebeln.

              IF sy-subrc NE 0 OR e_lgort IS INITIAL.
                CONCATENATE 'Deposito não encontrado para o pedido:' wa_ekko-ebeln INTO msg_text SEPARATED BY space.
                wa_log_erros-type          = 'E'.
                wa_log_erros-number        = '000'.
                wa_log_erros-mensagem      = msg_text.
                wa_log_erros-ch_referencia = wa_roma_pro-ch_referencia.
                APPEND wa_log_erros TO it_log_erros.
                wa_roma_pro-remessa = v_deliv_numb.
                APPEND wa_roma_pro TO it_roma_log.
                EXIT.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
      ELSE.
        e_lgort = vl_ablad+0(4).
      ENDIF.

      "Residuo 23.05.2017
      CLEAR: it_setleaf[].

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_zsdt0001-matnr
        IMPORTING
          output = vmatnr.

      v_zsdt0001-matnr = vmatnr.

      IF v_zsdt0001-branch+0(2) NE '15'. "Agro não faz entrrada de residuo automatica
        SELECT * INTO TABLE it_setleaf
           FROM setleaf
           WHERE setname EQ 'RESIDUO'
             AND valfrom EQ v_zsdt0001-matnr.
        "
        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM zmmt0074
            INTO sl_zmmt0074
            WHERE werks = v_zsdt0001-branch
            AND   matnr = v_zsdt0001-matnr.
          IF sy-subrc EQ 0.
            PERFORM z_entrada_residuo USING v_zsdt0001 vl_mat_doc_r vl_matdocumentyear_r e_lgort CHANGING vl_error_ent_r.

            IF vl_error_ent_r IS NOT INITIAL.
              wa_roma_pro-remessa = v_deliv_numb.
              APPEND wa_roma_pro TO it_roma_log.
              EXIT.
            ENDIF.
          ELSE.
            CONCATENATE 'Material  obrigatório parâmetro Entrada Residuo, procure Depto Estoque' '!' INTO msg_text SEPARATED BY space.
            wa_log_erros-type          = 'E'.
            wa_log_erros-number        = '000'.
            wa_log_erros-mensagem      = msg_text.
            wa_log_erros-ch_referencia = wa_roma_pro-ch_referencia.
            APPEND wa_log_erros TO it_log_erros.
            wa_roma_pro-remessa = v_deliv_numb.
            APPEND wa_roma_pro TO it_roma_log.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
      "fim residuo

      it_itens-ref_doc        = wa_ekko-ebeln.
      it_itens-ref_item       = wa_ekko-ebelp.
      it_itens-dlv_qty        = wa_roma_pro-peso_liq.
      it_itens-sales_unit     = wa_ekko-meins.
      it_itens-sales_unit_iso = wa_ekko-meins.
      IF v_user IS INITIAL.
        v_due_date  = sy-datum.
      ELSE.

        v_due_date  = v_zsdt0001-dt_movimento.

        IF v_zsdt0001-fat_contingencia_ecc EQ abap_true.
          DATA: lwa_faturamento_ecc TYPE zde_compare_faturamento.

          CALL FUNCTION 'ZLES_FAT_CONTINGENCIA_0002'
            EXPORTING
              i_ch_referencia         = v_zsdt0001-ch_referencia
              i_get_dados_fat_ecc     = abap_true
            IMPORTING
              e_dados_faturamento_ecc = lwa_faturamento_ecc.

          IF lwa_faturamento_ecc-data_lcto_nf IS INITIAL.
            MESSAGE 'Data Lacto NF-e não encontrado no ECC'  TYPE 'E'.
            RETURN.
          ENDIF.

          v_due_date = lwa_faturamento_ecc-data_lcto_nf.
        ENDIF.

        PERFORM memorizar_dt_movimento_badi USING v_due_date.
      ENDIF.

      APPEND it_itens.

      v_primeira_vez = 'X'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ekko-werks
        IMPORTING
          output = vl_kunnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_ekko-reswk
        IMPORTING
          output = vl_lifnr.

      IF vl_route IS INITIAL.
        CALL FUNCTION 'Z_LES_ITINERARIO_ROUTE'
          EXPORTING
            p_kunnr      = vl_kunnr
            p_lifnr      = vl_lifnr
          IMPORTING
            route_out    = vl_route
          EXCEPTIONS
            nao_cli      = 1
            nao_for      = 2
            nao_cli_zone = 3
            nao_for_zone = 4
            nao_itinera  = 5
            nao_zonas    = 6
            OTHERS       = 7.
      ENDIF.

      IF sy-subrc <> 0.
        it_retorno2-type    = 'E'.
        it_retorno2-number  = '000'.
        it_retorno2-message = sy-msgv1.
        APPEND it_retorno2.
        PERFORM f_estorno_res CHANGING v_zsdt0001.
      ELSE.

        it_retorno2-type    = 'S'.
        it_retorno2-number  = '000'.
        it_retorno2-message = sy-msgv1.
        APPEND it_retorno2.

        CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_STO' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            due_date          = v_due_date
          IMPORTING
            delivery          = v_deliv_numb
          TABLES
            stock_trans_items = it_itens
            return            = it_retorno
            deliveries        = it_items.

        READ TABLE it_retorno WITH KEY type = 'S'.

        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          v_rfmng = wa_roma_pro-peso_liq.
          wa_roma_pro-mark = 'X'.
          "Forçar a liberação do documento gerado
          CALL FUNCTION 'DEQUEUE_ALL'
            EXPORTING
              _synchron = 'X'.

          "GRAVA REFERENCIA ROMANEIO
          v_xblnr = v_zsdt0001-ch_referencia.
          CALL FUNCTION 'UPDATE_XBLNR_IN_LIKP'
            EXPORTING
              i_vbeln = v_deliv_numb
              i_xblnr = v_xblnr.

          CLEAR: vl_charg, it_retorno2[], header_partner[], item_data[], item_control[], item_data_spl[].

          wa_header_data-deliv_numb     = v_deliv_numb.
          wa_header_control-deliv_numb  = 'X'.

          header_partner-upd_mode_partn = 'I'.
          header_partner-deliv_numb     = v_deliv_numb.
          header_partner-itm_number     = '000010'.
          header_partner-partn_role     = 'PC'.
          header_partner-partner_no     = v_zsdt0001-parid.
          APPEND header_partner.

          header_partner-upd_mode_partn = 'I'.
          header_partner-deliv_numb     = v_deliv_numb.
          header_partner-itm_number     = '000010'.
          header_partner-partn_role     = 'SP'.
          header_partner-partner_no     = p_lifnr.
          APPEND header_partner.

          header_partner-upd_mode_partn = 'I'.
          header_partner-deliv_numb     = v_deliv_numb.
          header_partner-itm_number     = '000010'.
          header_partner-partn_role     = 'LR'.
          header_partner-partner_no     = v_zsdt0001-id_cli_dest.

          SELECT SINGLE *
            FROM ekpa INTO @DATA(wl_ekpa_tmp)
           WHERE ebeln EQ @wa_ekko-ebeln
             AND parvw EQ 'ZT'. "Transbordo

          IF ( sy-subrc EQ 0 ) AND ( wa_ekko-ebeln IS NOT INITIAL ) AND ( wa_ekko-bsart EQ 'ZUB' ) AND ( wl_ekpa_tmp-lifn2 IS NOT INITIAL ).
            SELECT SINGLE *
              FROM lfa1 INTO @DATA(wl_lfa1_tmp)
             WHERE lifnr EQ @wl_ekpa_tmp-lifn2.

            IF sy-subrc EQ 0.
              TRY.
                  zcl_clientes=>zif_parceiros~get_instance( )->set_parceiro_cnpj_cpf_ie(
                     EXPORTING
                       i_cnpj             =  CONV #( wl_lfa1_tmp-stcd1 )
                       i_insc_estatual    =  CONV #( wl_lfa1_tmp-stcd3 )
                  )->get_id_parceiro(  IMPORTING e_parceiro = DATA(_id_parceiro_zt) ).

                  header_partner-partner_no = _id_parceiro_zt.

                CATCH zcx_parceiros.
              ENDTRY.
            ENDIF.
          ENDIF.

          APPEND header_partner.

          DATA(_fertilizantes) = abap_false.
          SELECT SINGLE matkl INTO @DATA(matkl)
            FROM ekpo AS a INNER JOIN tvarvc AS b ON b~name  = 'MAGGI_GR_FERTILIZANTES'
                                                 AND a~matkl = b~low
           WHERE ebeln EQ @wa_ekko-ebeln.

          IF sy-subrc EQ 0 OR _biodiesel = abap_true.
            _fertilizantes = abap_true.
          ENDIF.

          item_data-deliv_numb          = v_deliv_numb.
          item_data-deliv_item          = '000010'.
          IF sy-tcode NE 'ZLES0115' AND _fertilizantes EQ abap_false.
            item_data-batch               = wa_roma_pro-nr_safra.
          ELSE.
            SELECT SINGLE charg
                  FROM eket
                  INTO vl_charg
                  WHERE ebeln = wa_ekko-ebeln.
            item_data-batch               = vl_charg.
          ENDIF.

          item_data-dlv_qty             = wa_roma_pro-peso_liq.
          item_data-dlv_qty_imunit      = wa_roma_pro-peso_liq.
          item_data-fact_unit_nom       = 1.
          item_data-fact_unit_denom     = 1.
          IF _biodiesel = abap_true.
            item_data-gross_wt            = v_zsdt0001-peso_liq.
            item_data-net_weight          = v_zsdt0001-peso_liq.
          ELSE.
            item_data-gross_wt            = wa_roma_pro-peso_liq.
            item_data-net_weight          = wa_roma_pro-peso_liq.
          ENDIF.
          APPEND item_data.

          item_control-deliv_numb       = v_deliv_numb.
          item_control-deliv_item       = '000010'.
          item_control-chg_delqty       = 'X'.
          item_control-volume_flg       = 'X'.
          item_control-net_wt_flg       = 'X'.
          item_control-gross_wt_flg     = 'X'.
          APPEND item_control.

          item_data_spl-deliv_numb      = v_deliv_numb.
          item_data_spl-deliv_item      = '000010'.
          item_data_spl-stge_loc        = e_lgort.
          APPEND item_data_spl.

          CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
            EXPORTING
              header_data    = wa_header_data
              header_control = wa_header_control
              delivery       = v_deliv_numb
            TABLES
              header_partner = header_partner
              item_data      = item_data
              item_control   = item_control
              return         = it_retorno2
              item_data_spl  = item_data_spl.

          nlinhas = 0.
          DESCRIBE TABLE it_retorno2 LINES nlinhas.

          IF nlinhas IS INITIAL.

            it_retorno2-type    = 'S'.
            it_retorno2-number  = '000'.
            it_retorno2-message = 'Remessa preparada para movimento de mercadoria!'.
            APPEND it_retorno2.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            sl_vbkok_wa-vbeln_vl    = v_deliv_numb.
            sl_vbkok_wa-vbeln       = v_deliv_numb.
            sl_vbkok_wa-wabuc       = 'X'.
            sl_vbkok_wa-wadat_ist   = v_due_date.
            sl_vbpok-vbeln_vl       = v_deliv_numb.
            sl_vbpok-posnr_vl       = '000010'.
            sl_vbpok-vbeln          = v_deliv_numb.
            sl_vbpok-posnn          = '000010'.
            sl_vbpok-matnr          = v_zsdt0001-matnr.
            IF _biodiesel = abap_true.
              sl_vbpok-pikmg          = wa_roma_pro-peso_liq.
            ELSE.
              sl_vbpok-pikmg          = v_zsdt0001-peso_liq. "aqui
            ENDIF.

            IF _fertilizantes EQ abap_true.

              SELECT SINGLE charg
                FROM eket
                INTO vl_charg
                WHERE ebeln = wa_ekko-ebeln.

              sl_vbpok-charg   = vl_charg.
              sl_vbpok-lgort   = e_lgort.

            ELSEIF sy-tcode NE 'ZLES0115'.
              sl_vbpok-charg        = v_zsdt0001-nr_safra.
              sl_vbpok-lgort        = e_lgort.
            ELSE.
              SELECT SINGLE charg
                FROM eket
                INTO vl_charg
                WHERE ebeln = wa_ekko-ebeln.
              sl_vbpok-charg          = vl_charg.
              sl_vbpok-lgort          = vl_ablad+0(4).
            ENDIF.
            sl_vbpok-brgew          = v_zsdt0001-peso_liq.
            sl_vbpok-ntgew          = v_zsdt0001-peso_liq.
            sl_vbpok-gewei          = 'KG'.
            sl_vbpok-vbtyp_n        = 'V'.
            APPEND sl_vbpok TO tl_vbpok.

            CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
              EXPORTING
                vbkok_wa                 = sl_vbkok_wa
                synchron                 = 'X'
                if_error_messages_send_1 = 'X'
              TABLES
                vbpok_tab                = tl_vbpok
                prot                     = tl_prot.

            IF NOT tl_prot[] IS INITIAL.
              v_erro = 'X'.
            ENDIF.

            IF v_erro IS INITIAL.

              it_retorno2-type    = 'S'.
              it_retorno2-number  = '000'.
              it_retorno2-message = 'Movimento de mercadoria efetuado!'.
              APPEND it_retorno2.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              "Forçar a liberação do documento gerado
              CALL FUNCTION 'DEQUEUE_ALL'
                EXPORTING
                  _synchron = 'X'.

            ENDIF.

            IF v_erro IS INITIAL.

              REFRESH tl_lines.
              CLEAR: tl_lines, st_header, st_lines.

              st_header-tdobject = 'VBBK'.
              st_header-tdname   = v_deliv_numb.
              st_header-tdid     = '0001'.
              st_header-tdspras  = 'P'.

              CONCATENATE 'Pesagem OPUS ChRef:' v_zsdt0001-ch_referencia INTO st_lines-tdline SEPARATED BY space.
              st_lines-tdformat  = '*'.
              APPEND st_lines TO tl_lines.

              CLEAR: st_lines.
              CONCATENATE 'Número do Romaneio:' v_zsdt0001-nr_romaneio INTO st_lines-tdline SEPARATED BY space.
              st_lines-tdformat  = '*'.
              APPEND st_lines TO tl_lines.

              CALL FUNCTION 'SAVE_TEXT'
                EXPORTING
                  header          = st_header
                  savemode_direct = 'X'
                TABLES
                  lines           = tl_lines
                EXCEPTIONS
                  id              = 1
                  language        = 2
                  name            = 3
                  object          = 4
                  OTHERS          = 5.

              IF sy-subrc EQ 0.
                it_retorno2-type    = 'S'.
                it_retorno2-number  = '000'.
                CONCATENATE 'Textos da remessa' v_deliv_numb 'atualizado!' INTO it_retorno2-message SEPARATED BY space.
                APPEND it_retorno2.
              ELSE.
                it_retorno2-type    = 'E'.
                it_retorno2-number  = '000'.
                CONCATENATE 'Textos da remessa' v_deliv_numb ' não atualizado!' INTO it_retorno2-message SEPARATED BY space.
                APPEND it_retorno2.
              ENDIF.

              UPDATE zsdt0001
                 SET status  = 'X'
                     doc_rem = v_deliv_numb
                     tknum   = vl_tknum
               WHERE ch_referencia EQ v_zsdt0001-ch_referencia.

              IF sy-tcode NE 'ZLES0106' AND sy-tcode NE 'ZLES0115' AND sy-tcode NE 'ZMM0127' AND sy-tcode NE 'ZLES0136' OR
                 vg_faturamento_autom = abap_true.  "*-#133089-21.02.2024-JT
                UPDATE zsdt0001 SET st_proc = '99'
                 WHERE ch_referencia EQ v_zsdt0001-ch_referencia.
              ENDIF.

              CALL FUNCTION 'ZSD_BLOQUEIO_ROMANEIO'
                EXPORTING
                  cd_referencia = v_zsdt0001-ch_referencia.
              it_retorno2-type    = 'S'.
              it_retorno2-number  = '000'.
              CONCATENATE 'Romaneio' v_zsdt0001-nr_romaneio 'atualizado com remessa!' INTO it_retorno2-message SEPARATED BY space.
              APPEND it_retorno2.

              IF v_nr_romaneio IS NOT INITIAL AND sy-calld = 'X'.
                vl_delivery_c = v_deliv_numb.
                SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD vl_delivery_c.
                EXIT.
              ENDIF.
            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
              PERFORM apaga_delivery USING v_deliv_numb.
              LOOP AT tl_prot INTO sl_prot.
                wa_log_erros-type     = sl_prot-msgty.
                wa_log_erros-number   = 'PIK'.
                MESSAGE ID sl_prot-msgid
                          TYPE sl_prot-msgty
                        NUMBER sl_prot-msgno
                          WITH sl_prot-msgv1
                               sl_prot-msgv2
                               sl_prot-msgv3
                               sl_prot-msgv4
                          INTO wa_log_erros-mensagem.

                APPEND wa_log_erros TO it_log_warni.
              ENDLOOP.
              PERFORM f_estorno_res CHANGING v_zsdt0001.
            ENDIF.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            PERFORM apaga_delivery USING v_deliv_numb.
            PERFORM f_estorno_res CHANGING v_zsdt0001.
          ENDIF.

        ELSE.
          wa_roma_pro-mark    = ' '.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          PERFORM f_estorno_res CHANGING v_zsdt0001.
        ENDIF.

        LOOP AT it_retorno.
          wa_log_erros-type          = it_retorno-type.
          wa_log_erros-number        = it_retorno-number.
          wa_log_erros-mensagem      = it_retorno-message.
          wa_log_erros-ch_referencia = wa_roma_pro-ch_referencia.
          APPEND wa_log_erros TO it_log_erros.
        ENDLOOP.

      ENDIF.
      LOOP AT it_retorno2.
        wa_log_erros-type          = it_retorno2-type.
        wa_log_erros-number        = it_retorno2-number.
        wa_log_erros-mensagem      = it_retorno2-message.
        wa_log_erros-ch_referencia = wa_roma_pro-ch_referencia.
        APPEND wa_log_erros TO it_log_erros.
      ENDLOOP.
    ENDIF.
    wa_roma_pro-remessa = v_deliv_numb.
    APPEND wa_roma_pro TO it_roma_log.
  ENDLOOP.

  IF v_nr_romaneio IS NOT INITIAL AND sy-calld = 'X'.
    IF it_roma_log[] IS NOT INITIAL.
      READ TABLE it_roma_log INTO wa_roma_log INDEX 1.
      READ TABLE it_log_warni INTO wa_log_erros
          WITH KEY ch_referencia = wa_roma_log-ch_referencia
                   type          = 'E'.
      IF sy-subrc NE 0.
        READ TABLE it_log_erros INTO wa_log_erros
         WITH KEY ch_referencia = wa_roma_log-ch_referencia
                     type          = 'E'.
      ENDIF.
      IF sy-subrc = 0.
        PERFORM mostra_log_erro.
      ENDIF.
    ENDIF.
    EXIT.
  ELSE.
    SET PF-STATUS 'FINAL'.

    NEW-PAGE LINE-SIZE 164
             LINE-COUNT 40.

    PERFORM imprime_roma_cab_detalhe.

    LOOP AT it_roma_log INTO wa_roma_log.
      PERFORM imprime_roma_detalhes USING wa_roma_log.

      HIDE: wa_roma_log-mark,
            wa_roma_log-ch_referencia,
            wa_roma_log-nr_romaneio,
            wa_roma_log-dt_movimento,
            wa_roma_log-nr_safra,
            wa_roma_log-id_cli_dest,
            wa_roma_log-name1,
            wa_roma_log-tp_frete,
            wa_roma_log-peso_liq,
            wa_roma_log-placa_cav,
            wa_roma_log-nr_ticket,
            wa_roma_log-remessa,
            wa_roma_log-lgort.

    ENDLOOP.
  ENDIF.

ENDFORM.                    " GERA_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_LOG_ERRO
*&---------------------------------------------------------------------*
*       Mostra Logs de Erros de Processamento de romaneio
*----------------------------------------------------------------------*
FORM mostra_log_erro .

  CONSTANTS: mensagem TYPE c LENGTH 75 VALUE 'Mensagem'.
  DATA:      romaneio TYPE c LENGTH 85.

  CONCATENATE 'Romaneio:' wa_roma_log-nr_romaneio INTO romaneio SEPARATED BY space.

  READ CURRENT LINE.

  SET PF-STATUS 'DIALOG'.
  WINDOW STARTING AT 5 3 ENDING AT 80 20.

  ULINE.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE:/001 romaneio.
  ULINE.
  WRITE:/001 'Tipo', 006 'Nr.', 011 mensagem.
  ULINE.

  LOOP AT it_log_erros INTO wa_log_erros WHERE ch_referencia EQ wa_roma_log-ch_referencia.

    IF wa_log_erros-type EQ 'S'.
      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    ENDIF.

    WRITE: /001 wa_log_erros-type, 006 wa_log_erros-number, 011 wa_log_erros-mensagem.

*-#133089-21.02.2024-JT-inicio
    IF vg_faturamento_autom = abap_true AND wa_log_erros-type = 'E'.
      DATA(l_mesg) = wa_log_erros-mensagem.
      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_log_erros-ch_referencia i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
    ENDIF.
*-#133089-21.02.2024-JT-fim

  ENDLOOP.

  LOOP AT it_log_warni INTO wa_log_erros WHERE ch_referencia EQ wa_roma_log-ch_referencia.

    IF wa_log_erros-type EQ 'S'.
      FORMAT COLOR COL_POSITIVE INTENSIFIED OFF.
    ELSE.
      FORMAT COLOR COL_NEGATIVE INTENSIFIED OFF.
    ENDIF.

    WRITE: /001 wa_log_erros-type, 006 wa_log_erros-number, 011 wa_log_erros-mensagem.

*-#133089-21.02.2024-JT-inicio
    IF vg_faturamento_autom = abap_true AND wa_log_erros-type = 'E'.
      l_mesg = wa_log_erros-mensagem.
      lc_faturamento_automatico->set_gravar_mensagem( i_ch_referencia = wa_log_erros-ch_referencia i_type = 'E' i_msg = CONV #( l_mesg ) i_status = 'REME' ).
    ENDIF.
*-#133089-21.02.2024-JT-fim

  ENDLOOP.

ENDFORM.                    " MOSTRA_LOG_ERRO

*&---------------------------------------------------------------------*
*&      Form  DESBLOQUEIA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM desbloqueia_dados .

  CALL FUNCTION 'ZDEQUEUE_ROMANEIOS_CENTRO_PED'
    EXPORTING
      v_mode   = 'X'
      v_mandt  = sy-mandt
      v_werkes = wa_ekko-reswk
      v_matnr  = wa_ekko-matnr.

  SET USER-COMMAND 'BACK'.

ENDFORM.                    " DESBLOQUEIA_DADOS

*&---------------------------------------------------------------------*
*&      Form  APAGA_DELIVERY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_V_DELIV_NUMB  text
*----------------------------------------------------------------------*
FORM apaga_delivery  USING p_v_deliv_numb TYPE bapishpdelivnumb-deliv_numb.

  sl_hdata-deliv_numb = p_v_deliv_numb.
  sl_hcont-deliv_numb = p_v_deliv_numb.
  sl_hcont-dlv_del    = 'X'.
  vl_delivery         = p_v_deliv_numb.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
    EXPORTING
      header_data    = sl_hdata
      header_control = sl_hcont
      delivery       = vl_delivery
    TABLES
      return         = tl_bapiret2.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

ENDFORM.                    " APAGA_DELIVERY

*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_TVARVC                                       *
*&---------------------------------------------------------------------*
*                             Verifica Usuário                         *
*----------------------------------------------------------------------*
FORM z_seleciona_tvarvc.

  DATA: wa_setleaf LIKE setleaf.

  CLEAR v_user.

*  SELECT SINGLE LOW
*    FROM TVARVC
*    INTO V_USER
*  WHERE  NAME EQ 'REM_DATA_RETROATIVA'
*    AND  LOW  EQ SY-UNAME.

  SELECT SINGLE *
    FROM setleaf
    INTO wa_setleaf
   WHERE setname = 'VF01_USUARIO'
     AND valfrom = sy-uname.

  IF sy-subrc  IS INITIAL.
    v_user = sy-uname.
  ENDIF.

ENDFORM.                    " Z_SELECIONA_TVARVC

*&---------------------------------------------------------------------*
*&      Form  MEMORIZAR_DT_MOVIMENTO_BADI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SL_DATA_REM  text
*----------------------------------------------------------------------*
FORM memorizar_dt_movimento_badi  USING p_data_rem TYPE ledat.

  TYPES:
    BEGIN OF tab_type,
      para TYPE string,
      dobj TYPE string,
    END OF tab_type.

  DATA: line TYPE tab_type,
        itab TYPE STANDARD TABLE OF tab_type,
        id   TYPE c LENGTH 10 VALUE 'ROMRETRO'.

  line-para = 'P1'.
  line-dobj = 'P_DATA_REM'.
  APPEND line TO itab.

  EXPORT (itab) TO MEMORY ID 'ROMRETRO'.

ENDFORM.                    " MEMORIZAR_DT_MOVIMENTO_BADI


*&---------------------------------------------------------------------*
*&      Form  Z_ENTRADA_RESIDUO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_entrada_residuo USING sl_zsdt0001        TYPE zsdt0001
                             vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc
                             vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year
                             p_lgort            TYPE lgort_d
                    CHANGING p_error.

  DATA: sl_zsdt0001_ent TYPE zsdt0001,
        v_nr_romaneio   TYPE zsdt0001-nr_romaneio.

  DATA: wa_goodsmvt_header TYPE bapi2017_gm_head_01,
        t_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
        wa_goodsmvt_item   TYPE bapi2017_gm_item_create,
        wa_code            TYPE bapi2017_gm_code,
        t_return           TYPE TABLE OF bapiret2 WITH HEADER LINE,
        wa_return          TYPE bapiret2.

  CHECK sl_zsdt0001-id_carga IS INITIAL.

  CLEAR: t_goodsmvt_item, t_return, p_error.

  CLEAR: vl_mat_doc, vl_matdocumentyear.

  SELECT SINGLE *
    FROM zsdt0001 INTO @DATA(_wl_0001)
   WHERE ch_referencia EQ @sl_zsdt0001-ch_referencia.

  IF ( sy-subrc = 0 ) AND
     ( _wl_0001-doc_material_e IS NOT INITIAL ) AND
     ( _wl_0001-ano_material_e IS NOT INITIAL ).
    vl_mat_doc         = _wl_0001-doc_material_e.
    vl_matdocumentyear = _wl_0001-ano_material_e.
    EXIT.
  ENDIF.

  wa_goodsmvt_header-pstng_date = sy-datum.
  wa_goodsmvt_header-doc_date   = sy-datum.
  wa_goodsmvt_header-header_txt = sl_zsdt0001-vbeln.

  wa_code-gm_code               = '05'.

*--> 15.06.2023 - Migration S4 – MIGNOW - Start
  "  wa_goodsmvt_item-material     = sl_zsdt0001-matnr.
  DATA(v_len) = strlen( sl_zsdt0001-matnr ).
  IF v_len > 18.
    wa_goodsmvt_item-material_long = sl_zsdt0001-matnr .
  ELSE.
    wa_goodsmvt_item-material = sl_zsdt0001-matnr .
  ENDIF.
*<-- 15.06.2023 - Migration S4 – MIGNOW – End
  wa_goodsmvt_item-plant        = sl_zsdt0001-branch.
  " WA_GOODSMVT_ITEM-STGE_LOC      = 'ARMZ'.
  "23.01.2018

*  SELECT SINGLE LGORT
*    INTO  WA_GOODSMVT_ITEM-STGE_LOC
*   FROM EKPO
*   WHERE EBELN = SL_ZSDT0001-VBELN.

  wa_goodsmvt_item-stge_loc     = p_lgort.
  wa_goodsmvt_item-batch        = sl_zsdt0001-nr_safra.

  wa_goodsmvt_item-move_type    = sl_zmmt0074-bwart. "ALRS 22/05/2017
  IF sl_zmmt0074-entrada_rom = 'S'. "Checa romaneio de entrada
    IF sl_zsdt0001-id_referencia IS INITIAL. "
      EXIT. "Não gera  doc. entrada
    ENDIF.
    DATA(_nr_romaneio) = sl_zsdt0001-id_referencia.

    SELECT SINGLE *
      FROM zsdt0001
      INTO sl_zsdt0001_ent
      WHERE bukrs         = sl_zsdt0001-bukrs
      AND   branch        = sl_zsdt0001-branch
      AND   tp_movimento  = 'E'
      AND   nr_romaneio   = _nr_romaneio
      "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
      AND   nr_safra      = sl_zsdt0001-nr_safra.

    IF sl_zsdt0001_ent-ch_refer_ent IS NOT INITIAL.
      CLEAR sl_zsdt0001_ent-peso_liq.
      SELECT SUM( peso_liq )
          FROM zsdt0001
          INTO sl_zsdt0001_ent-peso_liq
          WHERE bukrs         = sl_zsdt0001_ent-bukrs
          AND   branch        = sl_zsdt0001_ent-branch
          AND   tp_movimento  = 'E'
          AND   ch_refer_ent  = sl_zsdt0001_ent-ch_refer_ent
          "AND   DT_MOVIMENTO  = SL_ZSDT0001-DT_MOVIMENTO
          AND   nr_safra      = sl_zsdt0001_ent-nr_safra.
    ENDIF.

    IF sy-subrc =  0.
      IF  sl_zsdt0001-peso_liq GT sl_zsdt0001_ent-peso_liq.
        wa_goodsmvt_item-entry_qnt    = sl_zsdt0001-peso_liq - sl_zsdt0001_ent-peso_liq.
      ELSE.
        EXIT. "Não gera  doc. entrada
      ENDIF.
    ELSE.
      p_error = 'X'.
      EXIT.
    ENDIF.

  ELSE.

    wa_goodsmvt_item-entry_qnt    = sl_zsdt0001-peso_liq.

  ENDIF.

  wa_goodsmvt_item-gl_account = '0000341004'.
  APPEND wa_goodsmvt_item TO t_goodsmvt_item.

  CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
    EXPORTING
      goodsmvt_header  = wa_goodsmvt_header
      goodsmvt_code    = wa_code
    IMPORTING
      materialdocument = vl_mat_doc
      matdocumentyear  = vl_matdocumentyear
    TABLES
      goodsmvt_item    = t_goodsmvt_item
      return           = t_return.

  READ TABLE t_return WITH KEY type = 'E'.

  IF ( NOT sy-subrc IS INITIAL ) AND
     ( vl_mat_doc IS NOT INITIAL ) AND
     ( vl_matdocumentyear IS NOT INITIAL ).

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    UPDATE zsdt0001 SET doc_material_e = vl_mat_doc
                        ano_material_e = vl_matdocumentyear
     WHERE ch_referencia EQ sl_zsdt0001-ch_referencia.

    COMMIT WORK.

  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    p_error = 'X'.

    LOOP AT t_return INTO DATA(w_ret) .
      wa_log_erros-type          = w_ret-type.
      wa_log_erros-number        = w_ret-number.
      wa_log_erros-mensagem      = w_ret-message.
      wa_log_erros-ch_referencia = sl_zsdt0001-ch_referencia.
      APPEND wa_log_erros TO it_log_erros.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " Z_ENTRADA_RESIDUO

FORM f_estorno_res  CHANGING p_zsdt0001 TYPE zsdt0001.
  DATA: w_romaneio               TYPE zsdt0001,
        wa_mat_doc               TYPE bapi2017_gm_head_02-mat_doc,
        wa_doc_year              TYPE bapi2017_gm_head_02-doc_year,
        wa_pstng_date            TYPE bapi2017_gm_head_02-pstng_date,
        vg_invoicedocnumber_migo TYPE bapi2017_gm_head_ret,
        v_budat                  TYPE mkpf-budat,
        w_mseg                   TYPE mseg.

  DATA: wa_goodsmvt_header TYPE bapi2017_gm_head_01,
        t_goodsmvt_item    TYPE TABLE OF bapi2017_gm_item_create,
        wa_goodsmvt_item   TYPE bapi2017_gm_item_create,
        wa_code            TYPE bapi2017_gm_code,
        vl_mat_doc         TYPE bapi2017_gm_head_ret-mat_doc,
        vl_matdocumentyear TYPE bapi2017_gm_head_ret-doc_year,
        ti_zlest0100       TYPE TABLE OF zlest0100  WITH HEADER LINE,
        wa_zlest0100       TYPE zlest0100,
        t_return_vt        LIKE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: vl_ponteiro  TYPE zlest0100-cont.

  CONSTANTS: c_x  TYPE c VALUE 'X'.


  "Estorna entrada de Residuo
  SELECT SINGLE *
    FROM zsdt0001
    INTO w_romaneio
    WHERE ch_referencia = p_zsdt0001-ch_referencia.

  IF w_romaneio-doc_material_e IS NOT INITIAL. "doc. material entrada residuo existe
    REFRESH t_return_vt.
    SELECT SINGLE budat INTO v_budat
      FROM mkpf
      WHERE mblnr = w_romaneio-doc_material_e
      AND   mjahr = w_romaneio-ano_material_e.
    "
    wa_mat_doc      = w_romaneio-doc_material_e.
    wa_doc_year    	= w_romaneio-ano_material_e.
    wa_pstng_date   = v_budat.

    SELECT SINGLE *
      INTO w_mseg
      FROM mseg
      WHERE mblnr = w_romaneio-doc_material_e
      AND   mjahr = w_romaneio-ano_material_e
      AND   bwart = 'ZX1'. "inverte

    IF sy-subrc = 0.
      CLEAR: t_goodsmvt_item.
      wa_goodsmvt_header-pstng_date = v_budat.
      wa_goodsmvt_header-doc_date   = v_budat.
      wa_goodsmvt_header-header_txt = w_romaneio-vbeln.

      wa_code-gm_code               = '05'.

*--> 15.06.2023 - Migration S4 – MIGNOW - Start
      "wa_goodsmvt_item-MATERIAL = w_mseg-matnr .
      DATA(v_len1) = strlen( w_mseg-matnr ).
      IF v_len1 > 18.
        wa_goodsmvt_item-material_long = w_mseg-matnr .
      ELSE.
        wa_goodsmvt_item-material = w_mseg-matnr .
      ENDIF.
*<-- 15.06.2023 - Migration S4 – MIGNOW – End
      wa_goodsmvt_item-plant        = w_mseg-werks.
      wa_goodsmvt_item-stge_loc	    = w_mseg-lgort.
      wa_goodsmvt_item-batch        = w_mseg-charg.

      wa_goodsmvt_item-move_type    = 'ZX2'. "ALRS 24/05/2017
      wa_goodsmvt_item-entry_qnt    = w_mseg-menge.
      APPEND wa_goodsmvt_item TO t_goodsmvt_item.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          goodsmvt_header  = wa_goodsmvt_header
          goodsmvt_code    = wa_code
        IMPORTING
          materialdocument = vl_mat_doc
          matdocumentyear  = vl_matdocumentyear
        TABLES
          goodsmvt_item    = t_goodsmvt_item
          return           = t_return_vt.
    ELSE.
      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          materialdocument    = wa_mat_doc
          matdocumentyear     = wa_doc_year
          goodsmvt_pstng_date = wa_pstng_date
        IMPORTING
          goodsmvt_headret    = vg_invoicedocnumber_migo
        TABLES
          return              = t_return_vt.
    ENDIF.

    IF t_return_vt[] IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.
      UPDATE zsdt0001
           SET doc_material_e      = ''
               ano_material_e      = ''
         WHERE ch_referencia = p_zsdt0001-ch_referencia.
    ELSE.
      "gravar log
      READ TABLE t_return_vt WITH KEY type = 'E'.
      IF sy-subrc EQ 0.
        REFRESH ti_zlest0100.
        CLEAR vl_ponteiro.
        SELECT  MAX( cont )
         FROM zlest0100
         INTO vl_ponteiro
         WHERE ch_referencia = p_zsdt0001-ch_referencia.

        IF sy-subrc = 0.
          ADD 1 TO vl_ponteiro.
        ELSE.
          vl_ponteiro = 1.
        ENDIF.
        LOOP AT t_return_vt.
          wa_zlest0100-mandt      = sy-mandt.
          wa_zlest0100-ch_referencia   = p_zsdt0001-ch_referencia.
          wa_zlest0100-msgtyp     = 'E'.
          wa_zlest0100-msgspra    = sy-langu.
          wa_zlest0100-msgid      = 'LES'.
          wa_zlest0100-msgnr      = '000'.
          wa_zlest0100-msgv1      = t_return_vt-message.
          wa_zlest0100-data       = sy-datum.
          wa_zlest0100-hora       = sy-uzeit.
          wa_zlest0100-usuario    = sy-uname.
          wa_zlest0100-cont       = vl_ponteiro.

          APPEND wa_zlest0100 TO ti_zlest0100.
          ADD 1 TO vl_ponteiro.
        ENDLOOP.
        MODIFY zlest0100 FROM TABLE ti_zlest0100.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.
