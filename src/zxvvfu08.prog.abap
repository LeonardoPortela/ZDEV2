*&---------------------------------------------------------------------*
*&  Include           ZXVVFU08
*&---------------------------------------------------------------------*

DATA: SL_ACCIT   TYPE ACCIT,
      WA_SETLEAF TYPE SETLEAF.

DATA:  E_STATUS(1),
       E_MESSA(64).

CALL FUNCTION 'Z_CONTROLE_FECHAMES'
  EXPORTING
    I_BUKRS  = CVBRK-BUKRS
    I_DATA   = CVBRK-FKDAT
  IMPORTING
    E_STATUS = E_STATUS
    E_MESSA  = E_MESSA
  EXCEPTIONS
    ERROR    = 1
    OTHERS   = 2.

IF SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
IF  E_STATUS = 'E'.
  MESSAGE E000(Z01) WITH E_MESSA.
ENDIF.

SELECT SINGLE *
  FROM SETLEAF
  INTO WA_SETLEAF
 WHERE SETNAME = 'MAGGI_EMPRESA_EXTERIOR'
   AND VALFROM = CVBRK-BUKRS.

IF SY-SUBRC IS NOT INITIAL.

  SL_ACCIT-XREF3 = CVBRK-ZUKRI+07.
  MODIFY XACCIT FROM SL_ACCIT
    TRANSPORTING XREF3
    WHERE BSCHL EQ '01'
       OR BSCHL EQ '11'.

  CLEAR: WA_SETLEAF.
  SELECT SINGLE *
    FROM SETLEAF
    INTO WA_SETLEAF
   WHERE SETNAME = 'VF01_USUARIO'
     AND VALFROM = SY-UNAME.

  IF ( SY-TCODE = 'VF01' ) AND ( WA_SETLEAF IS INITIAL ).
    IF CVBRK-FKDAT NE SY-DATUM.
      MESSAGE E000(Z01) WITH 'A data do faturamento tem que ser ' SY-DATUM.
      STOP.
    ENDIF.
  ENDIF.

  TABLES: J_1BNFLIN.

  DATA: WA_J_1BNFLIN TYPE J_1BNFLIN,
        WA_ZCTE_CIOT TYPE ZCTE_CIOT.

  "CASE SY-TCODE.
  "  WHEN: 'VF11'.

  IF ( SY-TCODE EQ 'VF11' ) OR ( SY-TCODE NE 'VF01' ).

    IF CVBRK-SFAKN IS NOT INITIAL.

      SELECT SINGLE * FROM J_1BNFLIN INTO WA_J_1BNFLIN WHERE REFKEY EQ CVBRK-SFAKN.
      IF SY-SUBRC IS INITIAL.

*        "Manual de Orientação do Contribuinte
*        "4 Web Services ................................................................................................................ 028
*        "4.11 Web Service – NFeConsultaDest ............................................................................................ 104
*        "4.11.9 Recomendações para evitar o uso indevido ............................................................................... 110
*        "A análise do comportamento atual das aplicações das empresas (“aplicação cliente”) permite identificar algumas situações de “uso indevido”
*        "dos ambientes de autorização de Nota Fiscal Eletrônica mantidos pelas SEFAZ.
*        "Como exemplo maior do mau uso do ambiente de autorização, ressalta-se a falta de controle de algumas aplicações que entram em “loop”,
*        "consumindo recursos de forma indevida, sobrecarregando principalmente o canal de comunicação com a Internet.
*        "Para este Web Service de Consulta às operações destinadas serão mantidos controles para identificar as situações de uso indevido de
*        "sucessivas tentativas de busca de registros já disponibilizados anteriormente.
*        "As novas tentativas serão rejeitadas com o erro “656 – Rejeição: Consumo Indevido”.
*
*        SELECT SINGLE * INTO @DATA(WA_J_1BNFE_AVTIVE)
*          FROM J_1BNFE_ACTIVE
*         WHERE DOCNUM EQ @WA_J_1BNFLIN-DOCNUM.
*
*        IF SY-SUBRC IS INITIAL AND WA_J_1BNFE_AVTIVE-CODE EQ '656'.
*          MESSAGE E017(Z01).
*          STOP.
*        ENDIF.

        SELECT SINGLE * FROM ZCTE_CIOT INTO WA_ZCTE_CIOT WHERE DOCNUM EQ WA_J_1BNFLIN-DOCNUM.
        IF ( SY-SUBRC EQ 0 ).
          IF ( ( WA_ZCTE_CIOT-ST_CIOT NE ZCL_CIOT=>C_8 ) AND
               ( WA_ZCTE_CIOT-ST_CIOT NE ZCL_CIOT=>C_0 ) OR
               ( WA_ZCTE_CIOT-ST_CIOT NE ZCL_CIOT=>C_0 ) AND
               ( WA_ZCTE_CIOT-ST_CIOT NE ZCL_CIOT=>C_8 ) ) AND
             ( WA_ZCTE_CIOT-ST_CIOT NE ZCL_CIOT=>C_9 ) AND
             ( WA_ZCTE_CIOT-ST_CIOT NE ZCL_CIOT=>C_3 ).
            MESSAGE E000(Z01) WITH 'Necessário cancelar a viagem'.
            STOP.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDIF.
  "ENDCASE.

ENDIF.
