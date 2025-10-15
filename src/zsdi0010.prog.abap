*----------------------------------------------------------------------*
*                   B B K O   C O N S U L T I N G                      *
*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZSDI0010                                                *
* Descrição  : NF Terceiros                                            *
* Módulo     : SD                                Transação: ZSDT0018   *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Pathelle R C Morais                    Data: 15/09/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*
*                     Histórico das modificações                       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Autor      :                                        Data:            *
* Observações:                                                         *
*----------------------------------------------------------------------*

REPORT zsdi0010 NO STANDARD PAGE HEADING MESSAGE-ID sd.

TABLES: likp ,
        lv50c,
        vbuk .

*----------------------------------------------------------------------*
*                                 TYPES                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF type_cabec,
         nr_roma  TYPE znr_romaneio,
         pedido   TYPE vbeln,
         nr_nf    TYPE j_1bnfnum9,
         serie    TYPE j_1bseries,
         dt_emi   TYPE j_1bcredat,
         vl_prod  TYPE j_1bnetval,
         vl_nf    TYPE j_1bnetval,
         cfop     TYPE j_1bagn-cfop,
         peso     TYPE brgew_ap  ,
         chave    TYPE j_1b_nfe_access_key_dtel44,
         remet    TYPE lifnr,
         name1    TYPE name1,
         pin      TYPE char9,
       END   OF type_cabec,

       BEGIN OF type_msn,
         ordem       TYPE vbeln       ,
         tp_msn      TYPE bapi_mtype  ,
         messagem    TYPE bapi_msg    ,
       END   OF type_msn.

*----------------------------------------------------------------------*
*                               ESTRUTURAS                             *
*----------------------------------------------------------------------*
DATA: s_cabec    TYPE type_cabec                 ,
      s_vbak     TYPE vbak                       ,
      s_vbap     TYPE vbap                       ,
      v_delivery TYPE bapishpdelivnumb-deliv_numb.

*----------------------------------------------------------------------*
*                                TABELAS                               *
*----------------------------------------------------------------------*
DATA: t_msn TYPE TABLE OF type_msn.

*----------------------------------------------------------------------*
*                            TELA DE SELEÇÂO                           *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.
SELECT-OPTIONS:
  s_vstel FOR likp-vstel  NO INTERVALS NO-EXTENSION,
  s_datbi FOR lv50c-datbi NO INTERVALS NO-EXTENSION DEFAULT sy-datum
                          OBLIGATORY,
  s_vbeln FOR vbuk-vbeln  NO INTERVALS NO-EXTENSION MATCHCODE OBJECT vmva
                          OBLIGATORY.
SELECTION-SCREEN END   OF BLOCK a2.
SELECTION-SCREEN BEGIN OF BLOCK a3 WITH FRAME TITLE text-002.
PARAMETERS:
  p_yes TYPE char1 RADIOBUTTON GROUP rg01 DEFAULT 'X' USER-COMMAND radio,
  p_no  TYPE char1 RADIOBUTTON GROUP rg01.
SELECTION-SCREEN END   OF BLOCK a3.
SELECTION-SCREEN END   OF BLOCK a1.

*----------------------------------------------------------------------*
*                         START OF SELECTION                           *
*----------------------------------------------------------------------*
START-OF-SELECTION.
* Verifica OV
  PERFORM: z_verifica_ov.

  CALL SCREEN 0100.

*&---------------------------------------------------------------------*
*&      Form  Z_VERIFICA_OV                                            *
*&---------------------------------------------------------------------*
*                                Verifica OV                           *
*----------------------------------------------------------------------*
FORM z_verifica_ov.

  CLEAR: s_vbak,
         s_vbap.

  SELECT SINGLE vbeln
    FROM vbap
    INTO s_vbeln-low
  WHERE  vbeln IN s_vbeln
    AND  vstel IN s_vstel.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE i836 WITH text-003.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT SINGLE *
    FROM vbak
    INTO s_vbak
  WHERE  vbeln IN s_vbeln.

  IF NOT s_vbak-zpesagem EQ '03'.
    MESSAGE i836 WITH text-004.
    LEAVE LIST-PROCESSING.
  ENDIF.

  SELECT SINGLE *
    FROM vbap
    INTO s_vbap
  WHERE  vbeln IN s_vbeln.

  s_vstel-low = s_vbap-vstel.

ENDFORM.                    " Z_VERIFICA_OV

*&---------------------------------------------------------------------*
*&      Module  ZM_STATUS  OUTPUT                                      *
*&---------------------------------------------------------------------*
*                                  Status                              *
*----------------------------------------------------------------------*
MODULE zm_status OUTPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      SET PF-STATUS 'PF0100'.
      SET TITLEBAR 'TB0100'.
  ENDCASE.

  LOOP AT SCREEN.
    IF screen-group1 EQ 'GR1'.
      IF p_no IS INITIAL.
        IF screen-group2 NE 'GR2'.
          screen-input = 0.
        ELSE.
          screen-required = 1.
        ENDIF.
        MODIFY SCREEN.
      ELSE.
        screen-required = 1.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-group1 EQ space AND
         screen-group2 EQ 'GR2' AND
         p_yes         IS INITIAL.
        screen-input    = 0.
        screen-required = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDMODULE.                 " ZM_STATUS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_USER_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              User Command                            *
*----------------------------------------------------------------------*
MODULE zm_user_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
      CASE sy-ucomm.
        WHEN 'SAVE'.
*         Gera Remessa
          PERFORM z_gera_remessa.
      ENDCASE.
  ENDCASE.

  CLEAR sy-ucomm.

ENDMODULE.                 " ZM_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Module  ZM_EXIT_COMMAND  INPUT                                 *
*&---------------------------------------------------------------------*
*                              Exit Command                            *
*----------------------------------------------------------------------*
MODULE zm_exit_command INPUT.

  CASE sy-dynnr.
    WHEN '0100'.
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
*&      Form  Z_GERA_REMESSA                                           *
*&---------------------------------------------------------------------*
*                               Gera Remessa                           *
*----------------------------------------------------------------------*
FORM z_gera_remessa.

* Cria Remessa
  PERFORM z_cria_remessa.

ENDFORM.                    " Z_GERA_REMESSA

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_REMESSA                                           *
*&---------------------------------------------------------------------*
*                                Cria Remessa                          *
*----------------------------------------------------------------------*
FORM z_cria_remessa.

  DATA: tl_item      TYPE TABLE OF bapidlvreftosalesorder,
        sl_item      TYPE bapidlvreftosalesorder         ,
        tl_return    TYPE TABLE OF bapiret2              .

  REFRESH: tl_item  ,
           tl_return.
  CLEAR: v_delivery ,
         sl_item    .

  sl_item-ref_doc        = s_vbap-vbeln.
  sl_item-ref_item       = s_vbap-posnr.
  sl_item-dlv_qty        = s_cabec-peso.
  sl_item-sales_unit     = s_vbap-vrkme.
  APPEND sl_item TO tl_item.

  CALL FUNCTION 'BAPI_OUTB_DELIVERY_CREATE_SLS'  "#EC CI_USAGE_OK[2438131]
    EXPORTING
      ship_point        = s_vstel-low
      due_date          = s_vbap-erdat
    IMPORTING
      delivery          = v_delivery
    TABLES
      sales_order_items = tl_item
      return            = tl_return.

  IF v_delivery IS INITIAL.
*   Retorna Erro
    PERFORM z_monta_erro TABLES tl_return.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*   Retorna Sucesso
    PERFORM: z_monta_sucesso USING v_delivery,
*   Picking
             z_picking       USING v_delivery.
  ENDIF.

  IF NOT t_msn[] IS INITIAL.
    CALL FUNCTION 'HR_IT_SHOW_ANY_TABLE_ON_ALV'
      TABLES
        table    = t_msn
      EXCEPTIONS
        fb_error = 1
        OTHERS   = 2.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  SUBMIT zsdi0010 VIA SELECTION-SCREEN.

ENDFORM.                    " Z_CRIA_REMESSA

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_ERRO                                             *
*&---------------------------------------------------------------------*
*                             Retorna Erro                             *
*----------------------------------------------------------------------*
FORM z_monta_erro TABLES p_return STRUCTURE bapiret2.

  DATA: sl_return TYPE bapiret2 ,
        sl_msn    TYPE type_msn .

  LOOP AT p_return INTO sl_return.
    sl_msn-ordem       = s_vbeln-low.
    sl_msn-tp_msn      = sl_return-type.
    sl_msn-messagem    = sl_return-message.
    APPEND sl_msn TO t_msn.
    CLEAR: sl_return,
           sl_msn   .
  ENDLOOP.

ENDFORM.                    " Z_MONTA_ERRO

*&---------------------------------------------------------------------*
*&      Form  Z_MONTA_SUCESSO                                          *
*&---------------------------------------------------------------------*
*                            Retorna Sucesso                           *
*----------------------------------------------------------------------*
FORM z_monta_sucesso USING p_delivery TYPE vbeln_vl.

  DATA sl_msn TYPE type_msn.

  sl_msn-ordem       = s_vbeln-low.
  sl_msn-tp_msn      = 'S'.
  CONCATENATE text-005
              p_delivery
         INTO sl_msn-messagem SEPARATED BY space.
  APPEND sl_msn TO t_msn.

ENDFORM.                    " Z_MONTA_SUCESSO

*&---------------------------------------------------------------------*
*&      Form  Z_PICKING                                                *
*&---------------------------------------------------------------------*
*                                  Picking                             *
*----------------------------------------------------------------------*
FORM z_picking USING p_delivery TYPE vbeln_vl.

  DATA: sl_vbkok_wa TYPE vbkok         ,
        tl_vbpok    TYPE TABLE OF vbpok,
        tl_prot     TYPE TABLE OF prott,
        sl_vbpok    TYPE vbpok         ,
        sl_prot     TYPE prott         ,
        tl_return   TYPE bapiret2_t    ,
        sl_return   TYPE bapiret2      ,
        vl_msn      TYPE char200       .

  REFRESH: tl_vbpok ,
           tl_return.

  sl_vbkok_wa-vbeln_vl  = p_delivery.
  sl_vbkok_wa-vbeln     = p_delivery.
  sl_vbkok_wa-wabuc     = 'X'.
  sl_vbkok_wa-wadat_ist = sy-datum.

  sl_vbpok-vbeln_vl       = p_delivery.
  sl_vbpok-posnr_vl       = s_vbap-posnr.
  sl_vbpok-vbeln          = p_delivery.
  sl_vbpok-posnn          = s_vbap-posnr.
  sl_vbpok-matnr          = s_vbap-matnr.
  sl_vbpok-pikmg          = s_cabec-peso.
  sl_vbpok-charg          = s_vbap-charg.
  APPEND sl_vbpok TO tl_vbpok.

  CALL FUNCTION 'SD_DELIVERY_UPDATE_PICKING_1'
    EXPORTING
      vbkok_wa                 = sl_vbkok_wa
      synchron                 = 'X'
      if_error_messages_send_1 = 'X'
    TABLES
      vbpok_tab                = tl_vbpok
      prot                     = tl_prot.

  IF tl_prot[] IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
*   Salva Dados Tabela ZSDT0009
    PERFORM z_salva_dados.
  ELSE.
*   Deleta Delivery Criado
    DATA: sl_hdata    TYPE bapiobdlvhdrchg,
          sl_hcont    TYPE bapiobdlvhdrctrlchg,
          vl_delivery TYPE bapiobdlvhdrchg-deliv_numb,
          tl_bapiret2 TYPE bapiret2_t.

    sl_hdata-deliv_numb = p_delivery.
    sl_hcont-deliv_numb = p_delivery.
    sl_hcont-dlv_del    = 'X'.
    vl_delivery         = p_delivery.
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

*   Retorna Menssagem
    DELETE tl_prot WHERE: posnr IS INITIAL,
                          msgty EQ 'S'    ,
                          msgty EQ 'W'    .
    IF NOT tl_prot[] IS INITIAL.
      LOOP AT tl_prot INTO sl_prot.
        MESSAGE ID sl_prot-msgid
          TYPE sl_prot-msgty
        NUMBER sl_prot-msgno
          WITH sl_prot-msgv1
               sl_prot-msgv2
               sl_prot-msgv3
               sl_prot-msgv4
          INTO sl_return-message.
        sl_return-type = sl_prot-msgty.
        APPEND sl_return TO tl_return.
        CLEAR: sl_prot  ,
               sl_return.
      ENDLOOP.
      sl_return-type = 'E'.
      CONCATENATE text-020
                  p_delivery
                  text-021
             INTO sl_return-message SEPARATED BY space.
      APPEND sl_return TO tl_return.
*     Retorna Erro
      PERFORM z_monta_erro TABLES tl_return.
    ENDIF.
  ENDIF.

ENDFORM.                    " Z_PICKING

*&---------------------------------------------------------------------*
*&      Form  Z_SALVA_DADOS                                            *
*&---------------------------------------------------------------------*
*                     Salva Dados Tabela ZSDT0009                      *
*----------------------------------------------------------------------*
FORM z_salva_dados.

  DATA sl_zsdt0009 TYPE zsdt0009.

  MOVE-CORRESPONDING s_cabec TO sl_zsdt0009.
  IF NOT p_yes IS INITIAL.
    sl_zsdt0009-nf = 'X'.
  ENDIF.
  sl_zsdt0009-vbeln_va = s_vbeln-low.
  sl_zsdt0009-vbeln_vl = v_delivery.

  INSERT zsdt0009 FROM sl_zsdt0009.

ENDFORM.                    "z_salva_dados

" Z_SALVA_DADOS
*&---------------------------------------------------------------------*
*&      Module  ZM_REMET  INPUT                                        *
*&---------------------------------------------------------------------*
*                                Remetente                             *
*----------------------------------------------------------------------*
MODULE zm_remet INPUT.

  CLEAR s_cabec-name1.

  CHECK NOT s_cabec-remet IS INITIAL.

  SELECT SINGLE lifnr name1
    FROM lfa1
    INTO (s_cabec-remet, s_cabec-name1)
  WHERE lifnr EQ s_cabec-remet.

  CHECK NOT sy-subrc IS INITIAL.
  MESSAGE e836 WITH text-006.

ENDMODULE.                 " ZM_REMET  INPUT
