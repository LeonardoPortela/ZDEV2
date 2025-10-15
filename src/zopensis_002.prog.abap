************************************************************************
* Program        : ZOPENSIS_002                                        *
* Transaction    : ZOPENSIS_002                                        *
* Title          : Cockipt de Ferramenta de Cadastro                   *
* Developer      : Fernando Oliveira                                   *
* Date           : 27/06/2019                                          *
* Project        :                                                     *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                  What                        *
* 1.00     27/06/2019 Fernando Oliveira   Início do Desenvolvimento    *
************************************************************************
PROGRAM zopensis_002.

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
DATA: gc_back.

*----------------------------------------------------------------------*
* Declaração de Constantes                                             *
*----------------------------------------------------------------------*
CONSTANTS: cc_perfil_relatorio       TYPE tvarvc-low VALUE 'ZOPENSIS_PERFIL_RELATORIO',
           cc_perfil_cadastrar_risco TYPE tvarvc-low VALUE 'ZOPENSIS_PERFIL_CADASTRA_RISCO',
           cc_perfil_carga_risco     TYPE tvarvc-low VALUE 'ZOPENSIS_PERFIL_CARGA_RISCO'.

*----------------------------------------------------------------------*
* Declaração de RANGES                                                 *
*----------------------------------------------------------------------*
DATA: lr_user_range TYPE RANGE OF char50,
      ls_user_name  LIKE LINE OF lr_user_range.

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
MODULE status_0100 OUTPUT.

  SET PF-STATUS '0100'.
  SET TITLEBAR  '0100'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE sy-ucomm.
    WHEN 'BACK' OR 'EXIT' OR 'CANC' .
      LEAVE TO TRANSACTION 'ZOPENSIS'.
    WHEN 'B1'.
      "SM30 - Cadastro dos Riscos
      CALL TRANSACTION 'ZOPENSIS_004'.

    WHEN 'B2'.
      "SM30 - Cadastro dos RiscosXTransação
      CALL TRANSACTION 'ZOPENSIS_005'.

    WHEN 'B3'.
      "Carga dos Riscos Compensatórios
      CALL TRANSACTION 'ZOPENSIS_006'.

    WHEN 'B4'.
      "Carga a Gestão dos Usuários
      CALL TRANSACTION 'ZOPENSIS_011'.

  ENDCASE.

ENDMODULE.
