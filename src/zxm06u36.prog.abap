*&---------------------------------------------------------------------*
*&  Include           ZXM06U36
*&---------------------------------------------------------------------*
TYPES:

  BEGIN OF TY_LFB1,
    LIFNR TYPE LFB1-LIFNR,
    BUKRS TYPE LFB1-BUKRS,
  END OF TY_LFB1,

  BEGIN OF TY_EKPO,
    EBELN TYPE EKPO-EBELN,
    EBELP TYPE EKPO-EBELP,
    LOEKZ TYPE EKPO-LOEKZ,
  END OF TY_EKPO.

DATA:
  BEGIN OF LS_EKBE,
    EBELN TYPE EKBE-EBELN,
    EBELP TYPE EKBE-EBELP,
  END OF LS_EKBE.


DATA:
  IT_LFB1       TYPE TABLE OF TY_LFB1,
  WA_LFB1       TYPE TY_LFB1,
  IT_EKPO       TYPE TABLE OF TY_EKPO,
  WA_EKPO       TYPE TY_EKPO,
  MSG_TXT01(50) TYPE C,
  MSG_TXT02(50) TYPE C.

DATA: T_BSART                 TYPE STANDARD TABLE OF  RGSB4 WITH HEADER LINE.

FIELD-SYMBOLS: <BEDAT> TYPE ANY.

"CS2018000533 - Impedir Alteração Campo
IF I_EKKO-BSART+0(3) = 'PCE'.
  IF I_EKKO-EBELN IS INITIAL.
    I_EKKO-BEDAT = SY-DATUM.
    ASSIGN ('(SAPLMEGUI)MEPO_TOPLINE-BEDAT') TO <BEDAT>.
    IF <BEDAT> IS ASSIGNED.
      <BEDAT> = SY-DATUM.
      MESSAGE I000(SU) WITH 'Não altere a data de ´pedido EXPRESSO!' .
    ENDIF.
  ENDIF.
ENDIF.

IF ( SY-TCODE NE 'ME41' ).
  IF I_EKKO-LIFNR IS NOT INITIAL  AND I_EKKO-BUKRS IS NOT INITIAL.
    SELECT LIFNR BUKRS
      FROM LFB1
      INTO TABLE IT_LFB1
    WHERE LIFNR EQ I_EKKO-LIFNR
      AND BUKRS EQ I_EKKO-BUKRS.


    IF ( ( SY-SUBRC <> 0 ) AND ( I_EKKO-BSART <> 'ZUB' ) ).

      CONCATENATE 'O Fornecedor' I_EKKO-LIFNR 'não está expandido para empresa' INTO MSG_TXT01 SEPARATED BY SPACE.
      CONCATENATE I_EKKO-BUKRS 'solicitar para Central de Cadastro.' INTO MSG_TXT02 SEPARATED BY SPACE.
      MESSAGE E000(SU) WITH MSG_TXT01 MSG_TXT02.

    ENDIF.
  ENDIF.

ENDIF.

**"---------------------------------------------------------------------
**" DETERMINAÇÃO  DO IVA DO PEDIDO
**"---------------------------------------------------------------------
CALL FUNCTION 'G_SET_GET_ALL_VALUES'
  EXPORTING
    CLASS         = '0000'
    SETNR         = 'MAGGI_ME21N_BSART'
  TABLES
    SET_VALUES    = T_BSART
  EXCEPTIONS
    SET_NOT_FOUND = 1
    OTHERS        = 2.
IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
SORT T_BSART BY FROM.
READ TABLE T_BSART WITH KEY FROM = I_EKKO-BSART.

*IF ( SY-TCODE EQ 'ME21N' OR
*     SY-TCODE EQ 'ME22N' OR
*     SY-TCODE EQ 'ME23N' ) AND SY-SUBRC NE 0.
*
*  DATA: VCATEG      TYPE ZMMT0089-CATEG,
*        VTP_PARC    TYPE ZMMT0089-TP_PARC,
*        VTP_PARC2   TYPE ZMMT0089-TP_PARC,
*        WL_ZMMT0089 TYPE ZMMT0089,
*        VWERKS      TYPE EKPO-WERKS,
*        V_CONTA     TYPE I,
*        V_CONTA2    TYPE I,
*        V_CONTA3    TYPE I,
*        VMTART(6).
*
*
*  FIELD-SYMBOLS: <W_ITEM>   TYPE BEKPO,
*                 <FS_CAMPO> TYPE ANY.
*  IF I_EKKO-LIFNR IS NOT INITIAL.
*
*    IF EKKO_CI-ZICMS IS INITIAL.
*      EKKO_CI-ZICMS  = I_EKKO-ZICMS.
*    ENDIF.
*    "
*    IF EKKO_CI-ZDESTI IS INITIAL.
*      EKKO_CI-ZDESTI = I_EKKO-ZDESTI.
*    ENDIF.
*    SELECT SINGLE *
*        FROM LFA1
*        INTO @DATA(W_LFA1)
*       WHERE LIFNR = @I_EKKO-LIFNR.
*    IF W_LFA1-REGIO EQ 'MT'.
*      VCATEG = '1'.
*    ELSE.
*      VCATEG = '2'.
*    ENDIF.
*    CLEAR VTP_PARC. " Branco -> pessoa jurídica
*    IF W_LFA1-STKZN IS NOT INITIAL. "pessoa física
*      VTP_PARC = 'PF'.
*    ENDIF.
*    LOOP AT TEKPO ASSIGNING <W_ITEM>.
*      IF <W_ITEM>-WERKS IS NOT INITIAL AND I_EKKO-BUKRS IS NOT INITIAL.
*        IF <W_ITEM>-BANFN IS NOT INITIAL.
*          SELECT SINGLE *
*            FROM EBAN
*            INTO @DATA(W_EBAN)
*            WHERE  BANFN   EQ @<W_ITEM>-BANFN
*            AND    ZDESTI NE ''.
*          IF SY-SUBRC = 0.
*            IF EKKO_CI-ZDESTI  NE W_EBAN-ZDESTI AND
*               EKKO_CI-ZDESTI  IS NOT INITIAL.
*              MESSAGE I000(SU) WITH 'Todas as Req. de compra devem ter a mesma destinação!' <W_ITEM>-EBELP.
*            ELSE.
*              EKKO_CI-ZDESTI = W_EBAN-ZDESTI.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*        SELECT SINGLE *
*          FROM MARA
*          INTO @DATA(W_MARA)
*        WHERE MATNR = @<W_ITEM>-MATNR.
*        CONCATENATE '%' W_MARA-MTART '%' INTO VMTART.
*        "
*        SELECT SINGLE *
*          FROM MBEW
*          INTO @DATA(W_MBEW)
*        WHERE MATNR = @<W_ITEM>-MATNR
*        AND   BWKEY = @<W_ITEM>-WERKS.
*        "
*        DATA(VICMS) = EKKO_CI-ZICMS..
*
*        IF <W_ITEM>-PSTYP = '9'.
*          IF <W_ITEM>-MATNR IS INITIAL.
*            CONCATENATE '%' ' ' INTO VMTART.
*            CLEAR:  W_MARA, VICMS.
*            VICMS = 'N'.
*            CLEAR W_MBEW.
*            W_MBEW-MTUSE = '1'.
*          ENDIF.
*        ENDIF.
*        V_CONTA3 = 0.
*        DO 2 TIMES.
*          ADD 1 TO V_CONTA3.
*          IF V_CONTA3 EQ 1. "padrão checar parametro com o centro se não tiver checar sem o centro (Genérico)
*            VWERKS = <W_ITEM>-WERKS.
*          ELSE.
*            CLEAR VWERKS.
*          ENDIF.
*
*          IF VTP_PARC = 'PF'.
*            V_CONTA = 2.
*          ELSE.
*            V_CONTA = 1.
*          ENDIF.
*          V_CONTA2 = 0.
*          DO V_CONTA TIMES.
*            ADD 1 TO V_CONTA2.
*            IF V_CONTA2 EQ 1.
*              VTP_PARC2 = VTP_PARC.
*            ELSE.
*              CLEAR VTP_PARC2.
*            ENDIF.
*            SELECT SINGLE *
*              FROM ZMMT0089
*              INTO WL_ZMMT0089
*             WHERE BUKRS = I_EKKO-BUKRS
*             AND   WERKS = VWERKS
*             AND   MTUSE = W_MBEW-MTUSE
*             AND   DESTI = EKKO_CI-ZDESTI
*             AND   CATEG = VCATEG
*             AND   TP_PARC  = VTP_PARC2
*             AND   ICMS  = VICMS
*             AND   MATKL = W_MARA-MATKL
*             AND   TP_MATERIAL LIKE VMTART.
*
*            IF SY-SUBRC NE 0.
*              SELECT SINGLE *
*                FROM ZMMT0089
*                INTO WL_ZMMT0089
*                  WHERE BUKRS = I_EKKO-BUKRS
*                  AND   WERKS = VWERKS
*                  AND   MTUSE = W_MBEW-MTUSE
*                  AND   DESTI = EKKO_CI-ZDESTI
*                  AND   CATEG = VCATEG
*                  AND   TP_PARC  = VTP_PARC2
*                  AND   ICMS  = VICMS
*                  AND   MATKL = W_MARA-MATKL.
*              IF SY-SUBRC NE 0.
*                SELECT SINGLE *
*                  FROM ZMMT0089
*                  INTO WL_ZMMT0089
*                    WHERE BUKRS = I_EKKO-BUKRS
*                    AND   WERKS = VWERKS
*                    AND   MTUSE = W_MBEW-MTUSE
*                    AND   DESTI = EKKO_CI-ZDESTI
*                    AND   CATEG = VCATEG
*                    AND   TP_PARC  = VTP_PARC2
*                    AND   ICMS  = VICMS.
*                IF SY-SUBRC = 0.
*                  IF <W_ITEM>-MWSKZ NE WL_ZMMT0089-MWSKZ.
*                    MESSAGE W000(SU) WITH 'IVA correto para item ' <W_ITEM>-EBELP  WL_ZMMT0089-MWSKZ.
*                  ENDIF.
*                  EXIT.
*                ELSEIF  ( V_CONTA3 = 2 AND V_CONTA = 1 ) OR ( V_CONTA3 = 2 AND V_CONTA2 = 2 ) .
*                  MESSAGE W000(SU) WITH 'Não existe IVA configurado item  ' <W_ITEM>-EBELP  WL_ZMMT0089-MWSKZ.
*                ENDIF.
*              ELSE.
*                IF <W_ITEM>-MWSKZ NE WL_ZMMT0089-MWSKZ.
*                  MESSAGE W000(SU) WITH 'IVA correto para item ' <W_ITEM>-EBELP  WL_ZMMT0089-MWSKZ.
*                ENDIF.
*                EXIT.
*              ENDIF.
*            ELSE.
*              IF <W_ITEM>-MWSKZ NE WL_ZMMT0089-MWSKZ.
*                MESSAGE W000(SU) WITH 'IVA correto para item ' <W_ITEM>-EBELP  WL_ZMMT0089-MWSKZ.
*              ENDIF.
*              EXIT.
*            ENDIF.
*          ENDDO.
*        ENDDO.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.
*
*ENDIF.
