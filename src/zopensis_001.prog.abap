************************************************************************
* Program        : ZOPENSIS_001                                        *
* Transaction    : ZOPENSIS                                            *
* Title          : Cockpit do Controle Compensatório                   *
* Developer      : Fernando Oliveira                                   *
* Date           : 27/06/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     27/06/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************
PROGRAM ZOPENSIS_001.

*----------------------------------------------------------------------*
* Tabelas SAP                                                          *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaração de Tipos                                                  *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaração de tabelas interna                                        *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaração de Work-áreas                                             *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaração de Variáveis                                              *
*----------------------------------------------------------------------*
DATA: GC_BACK.

*----------------------------------------------------------------------*
* Declaração de Constantes                                             *
*----------------------------------------------------------------------*
CONSTANTS: CC_PERFIL_RELATORIO       TYPE TVARVC-LOW VALUE 'ZOPENSIS_PERFIL_RELATORIO',
           CC_PERFIL_CADASTRAR_RISCO TYPE TVARVC-LOW VALUE 'ZOPENSIS_PERFIL_CADASTRA_RISCO',
           CC_PERFIL_CARGA_RISCO     TYPE TVARVC-LOW VALUE 'ZOPENSIS_PERFIL_CARGA_RISCO',
           CC_LINK_GESTAO_RISCO      TYPE TVARVC-LOW VALUE 'ZOPENSIS_LINK_GESTAO_RISCO'.

*----------------------------------------------------------------------*
* Declaração de RANGES                                                 *
*----------------------------------------------------------------------*
DATA: LR_USER_RANGE TYPE RANGE OF CHAR50,
      LS_USER_NAME  LIKE LINE OF LR_USER_RANGE.

*----------------------------------------------------------------------*
* Start-Of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CALL SCREEN '0100'.

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  DATA: L_OBJ TYPE SMENSAPNEW-REPORT,
        L_REP TYPE SMENSAPNEW-REPORTTYPE,
        L_URL TYPE SMEN_BUFFI-URL.

  CASE SY-UCOMM.
    WHEN 'BACK' OR 'EXIT' OR 'CANC' .
      LEAVE PROGRAM.

*----------------------------------------------------------------------*
* ZOPENSIS_REL_001 - Relatório de Controle Compensatório               *
*----------------------------------------------------------------------*
    WHEN 'B1'.

      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_PERFIL_RELATORIO.

      IF SY-SUBRC = 0.

        READ TABLE LR_USER_RANGE INTO LS_USER_NAME WITH KEY LOW = SY-UNAME.
        IF SY-SUBRC = 0.
          CALL TRANSACTION 'ZOPENSIS_001'.
        ELSE.
          MESSAGE 'Usuário sem permissão para essa funcionalidade.' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Nenhum Usuário atribuido para essa função' TYPE 'E'.
      ENDIF.

*----------------------------------------------------------------------*
* /VIRSA/ZVRAT - Transação de Controle Compensatórios
*----------------------------------------------------------------------*
    WHEN 'B2'.
      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_PERFIL_RELATORIO.

      IF SY-SUBRC = 0.
        READ TABLE LR_USER_RANGE INTO LS_USER_NAME WITH KEY LOW = SY-UNAME.
        IF SY-SUBRC = 0.
          PERFORM ZF_VIRSA.
        ELSE.
          MESSAGE 'Usuário sem permissão para essa funcionalidade.' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Nenhum Usuário atribuido para essa função' TYPE 'E'.
      ENDIF.

*----------------------------------------------------------------------*
* ZOPENSIS_REL_002 - Relatório dos Riscos Justificaveis                *
*----------------------------------------------------------------------*
    WHEN 'B3'.

      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_PERFIL_RELATORIO.

      IF SY-SUBRC = 0.

        READ TABLE LR_USER_RANGE INTO LS_USER_NAME WITH KEY LOW = SY-UNAME.
        IF SY-SUBRC = 0.
          CALL TRANSACTION 'ZOPENSIS_003'.
        ELSE.
          MESSAGE 'Usuário sem permissão para essa funcionalidade.' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Nenhum Usuário atribuido para essa função' TYPE 'E'.
      ENDIF.

*----------------------------------------------------------------------*
* ZOPENSIS_002 - Ferramentas de Cadastro / Carga                       *
*----------------------------------------------------------------------*
    WHEN 'FERRAMENTA'.

      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_PERFIL_CARGA_RISCO.

      IF SY-SUBRC = 0.

        READ TABLE LR_USER_RANGE INTO LS_USER_NAME WITH KEY LOW = SY-UNAME.
        IF SY-SUBRC = 0.
          CALL TRANSACTION 'ZOPENSIS_002'.
        ELSE.
          MESSAGE 'Usuário sem permissão para essa funcionalidade.' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Nenhum Usuário atribuido para essa função' TYPE 'E'.
      ENDIF.

*----------------------------------------------------------------------*
* ZOPENSIS_REL_003 - Aprovação - Controle Compensatório                           *
*----------------------------------------------------------------------*
    WHEN 'B4'.
      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_PERFIL_RELATORIO.

      IF SY-SUBRC = 0.

        READ TABLE LR_USER_RANGE INTO LS_USER_NAME WITH KEY LOW = SY-UNAME.
        IF SY-SUBRC = 0.
          SET PARAMETER ID 'FIRE' FIELD 'X'.
          CALL TRANSACTION 'ZOPENSIS_001'.
        ELSE.
          MESSAGE 'Usuário sem permissão para essa funcionalidade.' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Nenhum Usuário atribuido para essa função' TYPE 'E'.
      ENDIF.

*----------------------------------------------------------------------*
*                             *
*----------------------------------------------------------------------*
    WHEN 'B6'.
      CALL TRANSACTION 'ZOPNSS_SURVEY'.

*----------------------------------------------------------------------*
*                       *
*----------------------------------------------------------------------*
    WHEN 'B5'.

      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_LINK_GESTAO_RISCO.

      IF SY-SUBRC = 0.
        READ TABLE LR_USER_RANGE INTO LS_USER_NAME INDEX 1.
        IF SY-SUBRC = 0.
          L_URL = LS_USER_NAME.
        ELSE.
          L_URL = 'http://134.209.74.51:3333/login'.
        ENDIF.
      ELSE.
        L_URL = 'http://134.209.74.51:3333/login'.
      ENDIF.

      L_OBJ = 'URL'.
      L_REP = 'OT'.

      CALL FUNCTION 'MENU_START_OBJECT_VIA_INDX'
        EXPORTING
          OBJECT_NAME = L_OBJ
          REPORTTYPE  = L_REP
          URL         = L_URL.

*----------------------------------------------------------------------*
* ZOPENSIS_REL_003 - Relatório Firefighter                             *
*----------------------------------------------------------------------*
    WHEN 'B7'.
      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_PERFIL_RELATORIO.

      IF SY-SUBRC = 0.

        READ TABLE LR_USER_RANGE INTO LS_USER_NAME WITH KEY LOW = SY-UNAME.
        IF SY-SUBRC = 0.
          SET PARAMETER ID 'FIRE' FIELD 'X'.
          CALL TRANSACTION 'ZOPENSIS_007'.
        ELSE.
          MESSAGE 'Usuário sem permissão para essa funcionalidade.' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Nenhum Usuário atribuido para essa função' TYPE 'E'.
      ENDIF.

    WHEN 'BT1'.
      CALL TRANSACTION 'ZOPENSIS_010'.
    WHEN 'BT2'.
      CALL TRANSACTION 'ZOPENSIS_009'.

*----------------------------------------------------------------------*
* ZOPENSIS_REL_006 - Relatório de Comparação - Usuário x Revisão Perfil*
*----------------------------------------------------------------------*
    WHEN 'B8'.
      FREE LR_USER_RANGE[].
      SELECT SIGN OPTI LOW HIGH
        FROM TVARVC
        INTO TABLE LR_USER_RANGE
        WHERE NAME = CC_PERFIL_RELATORIO.

      IF SY-SUBRC = 0.

        READ TABLE LR_USER_RANGE INTO LS_USER_NAME WITH KEY LOW = SY-UNAME.
        IF SY-SUBRC = 0.
          CALL TRANSACTION 'ZOPENSIS_012'.
        ELSE.
          MESSAGE 'Usuário sem permissão para essa funcionalidade.' TYPE 'E'.
        ENDIF.
      ELSE.
        MESSAGE 'Nenhum Usuário atribuido para essa função' TYPE 'E'.
      ENDIF.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  ZF_VIRSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM ZF_VIRSA .

  DATA: GT_BDCDATA TYPE STANDARD TABLE OF BDCDATA,
        GW_BDCDATA TYPE BDCDATA.

  SET PARAMETER ID 'ZOPENSIS' FIELD 'ZOPENSIS'.

  GW_BDCDATA-PROGRAM = '/VIRSA/ZVRAT'.
  GW_BDCDATA-DYNPRO = '9100'.
  GW_BDCDATA-DYNBEGIN = 'X'.
  APPEND GW_BDCDATA TO GT_BDCDATA.

  CLEAR: GW_BDCDATA.
  GW_BDCDATA-FNAM = 'BDC_CURSOR'.
  GW_BDCDATA-FVAL = 'R3'.
  APPEND GW_BDCDATA TO GT_BDCDATA.

  CLEAR: GW_BDCDATA.
  GW_BDCDATA-FNAM = 'BDC_OKCODE'.
  GW_BDCDATA-FVAL = '=S_EXEC'.
  APPEND GW_BDCDATA TO GT_BDCDATA.

  CLEAR: GW_BDCDATA.
  GW_BDCDATA-FNAM = 'R3'.
  GW_BDCDATA-FVAL = 'X'.
  APPEND GW_BDCDATA TO GT_BDCDATA.

  CLEAR: GW_BDCDATA.
  GW_BDCDATA-FNAM = 'S_AUD'.
  GW_BDCDATA-FVAL = 'X'.
  APPEND GW_BDCDATA TO GT_BDCDATA.

  CALL TRANSACTION '/VIRSA/ZVRAT' USING GT_BDCDATA
       MODE 'E'.

ENDFORM.
