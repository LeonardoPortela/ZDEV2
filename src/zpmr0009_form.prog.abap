*&---------------------------------------------------------------------*
*&  Include           ZPMR0009_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  F_BUSCAR_EQUIPAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_EQUNR  text
*----------------------------------------------------------------------*
FORM F_BUSCAR_EQUIPAMENTO USING P_EQUNR.
  FREE OBJ_EQUIPAMENTO.
  CREATE OBJECT OBJ_EQUIPAMENTO.
  CONDENSE P_EQUNR NO-GAPS.
  OBJ_EQUIPAMENTO->CONSULTAR_EQUIPAMENTO( EXPORTING P_EQUNR   = P_EQUNR
                                          CHANGING  C_INATIVO = GV_EQUIP_INAT ).

  CLEAR GW_TELA.

  MOVE-CORRESPONDING OBJ_EQUIPAMENTO->GW_EQUIPAMENTO TO GW_TELA.
ENDFORM.                    " F_BUSCAR_EQUIPAMENTO

*&---------------------------------------------------------------------*
*&      Form  f_montar_alv
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TIPO     text
*----------------------------------------------------------------------*
FORM F_MONTAR_ALV USING P_TIPO.
  FREE GT_FIELDCAT.
  CASE P_TIPO.
    WHEN 'O'.
      GW_MSG_AVISO = 'Foram encontrados ordens pendentes para este equipamento.'.
      F_PREENCHE_FIELDCAT:
         1  'GT_RESULT' 'ORDERID'     'Número da Ordem'           ''  ''  '',
         2  'GT_RESULT' 'ORDER_TYPE'  'Tipo da ordem'             ''  ''  '',
         3  'GT_RESULT' 'SHORT_TEXT'  'Descrição da Ordem'        ''  ''  '',
         4  'GT_RESULT' 'EQUIPMENT'   'Nr do equipamento'         ''  ''  '',
         5  'GT_RESULT' 'EQUIDESCR'   'Descrição do equipamento'  ''  ''  ''.
    WHEN 'N'.
      GW_MSG_AVISO = 'Foram encontrados notas pendentes para este equipamento.'.
      F_PREENCHE_FIELDCAT:
         1  'GT_NOTIFICATION' 'NOTIFICAT'   'Número da nota'      ''  ''  '',
         2  'GT_NOTIFICATION' 'NOTIF_TYPE'  'Tipo da nota'        ''  ''  ''.
  ENDCASE.

ENDFORM.                    "f_montar_alv

*&---------------------------------------------------------------------*
*&      Form  F_CHAMAR_TELA_AVISO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_TELA_AVISO USING P_TIPO.
  GV_TIPO = P_TIPO.
  CALL SCREEN 2000 STARTING AT 5 10.
ENDFORM.                    " F_CHAMAR_TELA_AVISO

*&---------------------------------------------------------------------*
*&      Form  F_ELIMINAR_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_TELA_EQUNR  text
*----------------------------------------------------------------------*
FORM F_TELA_ELIMINAR.
  DATA: LV_RESULT    TYPE C,
        LV_CONT_ERRO TYPE I.

  IF OBJ_EQUIPAMENTO IS INITIAL.
    MESSAGE TEXT-003 TYPE 'E'.
  ELSEIF GW_TELA-EQUI_TEXT = 'MREL'.
    MESSAGE TEXT-004 TYPE 'E'.
  ENDIF.

** Checa se existem notas e ordens pendentes para o equipamento
  OBJ_EQUIPAMENTO->CONSULTAR_ORDENS_PEND( GW_TELA-EQUNR ).
  IF OBJ_EQUIPAMENTO->GT_RESULT[] IS NOT INITIAL.
    ADD 1 TO LV_CONT_ERRO.
    ASSIGN OBJ_EQUIPAMENTO->GT_RESULT[] TO <FS_OUTTAB>.
    PERFORM F_TELA_AVISO USING 'O'.
  ENDIF.

  OBJ_EQUIPAMENTO->CONSULTAR_NOTAS_PEND( GW_TELA-EQUNR ).
  IF OBJ_EQUIPAMENTO->GT_NOTIFICATION[] IS NOT INITIAL.
    ADD 1 TO LV_CONT_ERRO.
    ASSIGN OBJ_EQUIPAMENTO->GT_NOTIFICATION[] TO <FS_OUTTAB>.
    PERFORM F_TELA_AVISO USING 'N'.
  ENDIF.

  IF LV_CONT_ERRO IS INITIAL.
    CLEAR GW_TL_ELIMINA.
    GW_TL_ELIMINA-EQUNR            = OBJ_EQUIPAMENTO->GW_EQUIPAMENTO-EQUNR.
    GW_TL_ELIMINA-EQTYP            = OBJ_EQUIPAMENTO->GW_EQUIPAMENTO-EQTYP.
    GW_TL_ELIMINA-TPLNR            = ''.
    GW_TL_ELIMINA-QMNUM            = ''.
    GW_TL_ELIMINA-CODE_ELIMINADOR  = ''.
    GW_TL_ELIMINA-CODE             = ''.
    GW_TL_ELIMINA-CODE_MOTIVO      = ''.
    GW_TL_ELIMINA-CODE_MOTIVO_DESC = ''.
    GW_TL_ELIMINA-ARBPL            = ''.
    GW_TL_ELIMINA-DATA             = SY-DATUM.

    CALL SCREEN 3000 STARTING AT 5 5.
  ELSE.
    MESSAGE 'Não é possível fazer a eliminação, existem pendências.' TYPE 'E'.
  ENDIF.
ENDFORM.                    " F_ELIMINAR_EQUIP

*&---------------------------------------------------------------------*
*&      Form  F_ELIMINAR_EQUIP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_GW_TL_ELIMINA_EQUNR  text
*----------------------------------------------------------------------*
FORM F_ELIMINAR_EQUIP  USING    P_GW_TL_ELIMINA_EQUNR.
  DATA: TL_RESULT       TYPE TABLE OF BAPI_ALM_ORDER_LISTHEAD_RESULT,
        TL_NOTIFICATION TYPE TABLE OF BAPI2080_1,

        WL_SUBEQUIP     TYPE TY_SUBEQUIP,
        WL_DIIMPT       TYPE DIIMPT,
        WL_PLANO        TYPE TY_IPML_SELECT,
        WL_VIQMEL       TYPE VIQMEL,

        LV_NUM_NOTA     TYPE C LENGTH 11,

        LV_MSG          TYPE C LENGTH 255,
        LV_MSG_ERRO     TYPE C LENGTH 255,

        LV_ERRO         TYPE I.

  IF  GW_TL_ELIMINA-EQUNR IS NOT INITIAL
  AND GW_TL_ELIMINA-EQTYP IS NOT INITIAL
  AND GW_TL_ELIMINA-TPLNR IS NOT INITIAL
  AND GW_TL_ELIMINA-CODE_ELIMINADOR IS NOT INITIAL
  AND GW_TL_ELIMINA-CODE IS NOT INITIAL
  AND GW_TL_ELIMINA-CODE_MOTIVO IS NOT INITIAL
  AND GW_TL_ELIMINA-ARBPL IS NOT INITIAL
  AND GW_TL_ELIMINA-DATA IS NOT INITIAL.

** Checa se há subequipamento
    FREE OBJ_SUB_EQUIPAMENTO.
    CREATE OBJECT OBJ_SUB_EQUIPAMENTO.

    OBJ_SUB_EQUIPAMENTO->CONSULTAR_SUBEQUIPAMENTO( EXPORTING P_EQUI_SUP = GW_TL_ELIMINA-EQUNR ).

    IF OBJ_SUB_EQUIPAMENTO->GT_SUBEQUIP IS NOT INITIAL.
      LOOP AT OBJ_SUB_EQUIPAMENTO->GT_SUBEQUIP INTO WL_SUBEQUIP.
**    Checa se existem notas para o equipamento
        OBJ_SUB_EQUIPAMENTO->CONSULTAR_NOTAS_PEND( WL_SUBEQUIP-EQUNR ).
        APPEND LINES OF OBJ_SUB_EQUIPAMENTO->GT_NOTIFICATION TO TL_NOTIFICATION.

**    Checa se existem ordem para o equipamento
        OBJ_SUB_EQUIPAMENTO->CONSULTAR_ORDENS_PEND( WL_SUBEQUIP-EQUNR ).
        APPEND LINES OF OBJ_SUB_EQUIPAMENTO->GT_RESULT TO TL_RESULT.

        OBJ_SUB_EQUIPAMENTO->CONSULTAR_PLANOS( WL_SUBEQUIP-EQUNR ).
      ENDLOOP.

      UNASSIGN <FS_OUTTAB>.
      IF TL_RESULT IS NOT INITIAL.
        ASSIGN TL_RESULT[] TO <FS_OUTTAB>.
        PERFORM F_TELA_AVISO USING 'O'.
        ADD 1 TO LV_ERRO.
      ENDIF.

      UNASSIGN <FS_OUTTAB>.
      IF TL_NOTIFICATION IS NOT INITIAL.
        ASSIGN TL_NOTIFICATION[] TO <FS_OUTTAB>.
        PERFORM F_TELA_AVISO USING 'N'.
        ADD 1 TO LV_ERRO.
      ENDIF.
    ENDIF.

    IF LV_ERRO IS INITIAL.
      IF OBJ_SUB_EQUIPAMENTO->GT_SUBEQUIP IS NOT INITIAL.
********************************  Eliminando subequipamentos *********************************
        LOOP AT OBJ_SUB_EQUIPAMENTO->GT_SUBEQUIP INTO WL_SUBEQUIP.
** Apontar sulco para pneus
          IF WL_SUBEQUIP-EQTYP EQ 'T'.
            OBJ_SUB_EQUIPAMENTO->APONTAR_SULCO_PNEUS( WL_SUBEQUIP-EQUNR ).
          ENDIF.

** Verifica pontos existente para o subequipamento
          OBJ_SUB_EQUIPAMENTO->CONSULTAR_PONTOS_EQUIP( WL_SUBEQUIP-EQUNR ).
          IF OBJ_SUB_EQUIPAMENTO->GT_DIIMPT IS NOT INITIAL.
** Desativa pontos
            LOOP AT OBJ_SUB_EQUIPAMENTO->GT_DIIMPT INTO WL_DIIMPT.
              OBJ_SUB_EQUIPAMENTO->DESATIVAR_PONTOS( EXPORTING P_POINT = WL_DIIMPT-POINT
                                                               P_EQUNR = WL_SUBEQUIP-EQUNR ).
            ENDLOOP.
          ENDIF.

** Verifica planos existente para o subequipamento
          OBJ_SUB_EQUIPAMENTO->CONSULTAR_PLANOS( WL_SUBEQUIP-EQUNR ).
          IF OBJ_SUB_EQUIPAMENTO->GT_SELC_PLANO IS NOT INITIAL.
            LOOP AT OBJ_SUB_EQUIPAMENTO->GT_SELC_PLANO INTO WL_PLANO.
              OBJ_SUB_EQUIPAMENTO->DESATIVAR_PLANOS( EXPORTING P_OBJNR = WL_PLANO-OBJNR
                                                               P_EQUNR = WL_SUBEQUIP-EQUNR ).
            ENDLOOP.
          ENDIF.

** Movimentar o equipamento para sucata
          OBJ_SUB_EQUIPAMENTO->MOVIMENTAR_EQUIPAMENTO( EXPORTING P_EQUNR = WL_SUBEQUIP-EQUNR
                                                                 P_LOCAL = GW_TL_ELIMINA-TPLNR ).

** Eliminar equipamento
          ADD 1 TO LV_NUM_NOTA.
          OBJ_SUB_EQUIPAMENTO->ELIMINAR_EQUIPAMENTO( EXPORTING P_OBJNR            = WL_SUBEQUIP-OBJNR
                                                               P_EQUNR            = WL_SUBEQUIP-EQUNR
                                                               P_CODE             = GW_TL_ELIMINA-CODE
                                                               P_CODE_MOTIVO      = GW_TL_ELIMINA-CODE_MOTIVO
                                                               P_CODE_ELIMINADOR  = GW_TL_ELIMINA-CODE_ELIMINADOR
                                                               P_CODE_NOTIFICA    = GW_TL_ELIMINA-CODE_NOTIFICA
                                                     IMPORTING E_VIQMEL = WL_VIQMEL ).
          IF WL_VIQMEL IS NOT INITIAL.
            IF LV_MSG IS INITIAL.
              LV_MSG = WL_VIQMEL-QMNUM.
            ELSE.
              CONCATENATE LV_MSG WL_VIQMEL-QMNUM INTO LV_MSG SEPARATED BY ','.
            ENDIF.

          ELSE.
            IF LV_MSG_ERRO IS INITIAL.
              LV_MSG_ERRO = WL_SUBEQUIP-EQUNR.
            ELSE.
              CONCATENATE LV_MSG_ERRO WL_SUBEQUIP-EQUNR INTO LV_MSG_ERRO SEPARATED BY ','.
            ENDIF.

          ENDIF.

        ENDLOOP.

      ENDIF.
******************************  Eliminando equipamento superior **********************************
      IF GW_TL_ELIMINA-EQTYP EQ 'T'.
        OBJ_EQUIPAMENTO->APONTAR_SULCO_PNEUS( GW_TL_ELIMINA-EQUNR ).
      ENDIF.

** Verifica pontos existente para o subequipamento
      OBJ_EQUIPAMENTO->CONSULTAR_PONTOS_EQUIP( GW_TL_ELIMINA-EQUNR ).
      IF OBJ_EQUIPAMENTO->GT_DIIMPT IS NOT INITIAL.
** Desativa pontos
        LOOP AT OBJ_EQUIPAMENTO->GT_DIIMPT INTO WL_DIIMPT.
          OBJ_EQUIPAMENTO->DESATIVAR_PONTOS( EXPORTING P_POINT = WL_DIIMPT-POINT
                                                       P_EQUNR = GW_TL_ELIMINA-EQUNR ).
        ENDLOOP.
      ENDIF.

** Verifica planos existente para o subequipamento
      OBJ_EQUIPAMENTO->CONSULTAR_PLANOS( GW_TL_ELIMINA-EQUNR ).
      IF OBJ_EQUIPAMENTO->GT_SELC_PLANO IS NOT INITIAL.
        LOOP AT OBJ_EQUIPAMENTO->GT_SELC_PLANO INTO WL_PLANO.
          OBJ_EQUIPAMENTO->DESATIVAR_PLANOS( EXPORTING P_OBJNR = WL_PLANO-OBJNR
                                                       P_EQUNR = GW_TL_ELIMINA-EQUNR ).
        ENDLOOP.
      ENDIF.

** Movimentar o equipamento para sucata
      OBJ_EQUIPAMENTO->MOVIMENTAR_EQUIPAMENTO( EXPORTING P_EQUNR = GW_TL_ELIMINA-EQUNR
                                                         P_LOCAL = GW_TL_ELIMINA-TPLNR ).

** Eliminar equipamento
      ADD 1 TO LV_NUM_NOTA.
      OBJ_EQUIPAMENTO->ELIMINAR_EQUIPAMENTO( EXPORTING P_OBJNR            = OBJ_EQUIPAMENTO->GW_EQUIPAMENTO-OBJNR
                                                       P_EQUNR            = OBJ_EQUIPAMENTO->GW_EQUIPAMENTO-EQUNR
                                                       P_CODE             = GW_TL_ELIMINA-CODE
                                                       P_CODE_MOTIVO      = GW_TL_ELIMINA-CODE_MOTIVO
                                                       P_CODE_ELIMINADOR  = GW_TL_ELIMINA-CODE_ELIMINADOR
                                                       P_CODE_NOTIFICA    = GW_TL_ELIMINA-CODE_NOTIFICA
                                             IMPORTING E_VIQMEL = WL_VIQMEL ).

      IF WL_VIQMEL IS NOT INITIAL.
        IF LV_MSG IS INITIAL.
          LV_MSG = WL_VIQMEL-QMNUM.
        ELSE.
          CONCATENATE LV_MSG WL_VIQMEL-QMNUM INTO LV_MSG SEPARATED BY ','.
        ENDIF.

      ELSE.
        IF LV_MSG_ERRO IS INITIAL.
          LV_MSG_ERRO = WL_SUBEQUIP-EQUNR.
        ELSE.
          CONCATENATE LV_MSG_ERRO WL_SUBEQUIP-EQUNR INTO LV_MSG_ERRO SEPARATED BY ','.
        ENDIF.

      ENDIF.

      IF LV_MSG IS INITIAL.
        MESSAGE 'Não foi possível eliminar.' TYPE 'I'.

        CONCATENATE 'Erro ao eliminar equipamento(s): ' LV_MSG_ERRO '.' INTO LV_MSG.
        MESSAGE LV_MSG TYPE 'I'.

      ELSE.
        CONCATENATE 'Notas de eliminação geradas: ' LV_MSG '.' INTO LV_MSG.
        MESSAGE LV_MSG TYPE 'I'.

        IF LV_MSG_ERRO IS NOT INITIAL.
          CONCATENATE 'Erro ao eliminar equipamento(s): ' LV_MSG_ERRO '.' INTO LV_MSG.
          MESSAGE LV_MSG TYPE 'I'.

        ENDIF.

      ENDIF.

      PERFORM F_BUSCAR_EQUIPAMENTO USING OBJ_EQUIPAMENTO->GW_EQUIPAMENTO-EQUNR.
      LEAVE TO SCREEN 0.

    ELSE.
      MESSAGE TEXT-003 TYPE 'E'.
    ENDIF.

  ELSE.
    MESSAGE 'Todos os campos são obrigatórios para eliminação' TYPE 'E' DISPLAY LIKE 'S'.
  ENDIF.

ENDFORM.                    " F_ELIMINAR_EQUIP

*&---------------------------------------------------------------------*
*&      Form  F_CARREGA_CAUSADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARREGAR_CAUSADOR.
  SELECT CODE KURZTEXT
    FROM QPCT
    INTO TABLE GT_QPCT_CAUS
    WHERE KATALOGART  = '5'
     AND  CODEGRUPPE = 'FPN-0010'
     AND  SPRACHE    = SY-LANGU.

ENDFORM.                    "F_CARREGA_CAUSADOR

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_MOTIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARREGAR_MOTIVO.
  SELECT CODE KURZTEXT
    FROM QPCT
    INTO TABLE GT_QPCT_MOT
    WHERE KATALOGART  = '5'
     AND  CODEGRUPPE = 'FPN-0020'
     AND  SPRACHE    = SY-LANGU.

ENDFORM.                    "F_CARREGAR_MOTIVO

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_ELIMINADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARREGAR_ELIMINADOR.
  SELECT CODE KURZTEXT
    FROM QPCT
    INTO TABLE GT_QPCT_ELIM
    WHERE KATALOGART  = 'D'
     AND  CODEGRUPPE = 'FPN-0010'
     AND  SPRACHE    = SY-LANGU.

ENDFORM.                    "F_CARREGAR_ELIMINADOR

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_local
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARREGAR_LOCAL USING    P_IWERK TYPE EQUZ-IWERK
                      CHANGING C_TPLNR TYPE TY_TL_ELIMINA-TPLNR.
  SELECT SINGLE TPLNR
    FROM IFLOT
    INTO GW_IFLOT
   WHERE IWERK = P_IWERK
     AND  FLTYP = 'X'.

  C_TPLNR = GW_IFLOT-TPLNR.

ENDFORM.                    "F_CARREGAR_local

*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_CENTRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CARREGAR_CENTRO.
  SELECT OBJID ARBPL
    FROM CRHD
    INTO TABLE GT_CRHD
    WHERE OBJTY = 'A'
     AND  VERWE = '0005'
     AND  WERKS = GW_TELA-IWERK.

ENDFORM.                    "F_CARREGAR_CENTRO
