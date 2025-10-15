*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Eduardo Ruttkowski Tavares                              &*
*& Data.....: 07/05/2014                                              &*
*& Descrição: Libro Mayor - Paraguay                                 &*
*& Transação: ZFI0054                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP                                                               &*
*&--------------------------------------------------------------------&*

REPORT  ZFIR0049.
TABLES BKPF.

TYPES: BEGIN OF TY_BKPF,
         BUKRS TYPE BKPF-BUKRS,
         GJAHR TYPE BKPF-GJAHR,
         BUDAT TYPE BKPF-BUDAT,
         BELNR TYPE BKPF-BELNR,
       END OF TY_BKPF.

*----------------------------------------------------------------------*
* Constantes
*----------------------------------------------------------------------*
CONSTANTS: C_X TYPE C VALUE 'X'.

*----------------------------------------------------------------------*
* TELA DE SELEÇÃO - FORMULARIO
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK B1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: S_BUKRS FOR BKPF-BUKRS NO-EXTENSION NO INTERVALS OBLIGATORY,
                S_BUDAT FOR BKPF-BUDAT NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK B1.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.


  DATA: VL_FORMNAME        TYPE TDSFNAME,
        VL_NAME            TYPE RS38L_FNAM,
        LS_CONTROL         TYPE SSFCTRLOP,
        LS_OPTIONS         TYPE SSFCOMPOP,
        JOB_OUTPUT_INFO    TYPE SSFCRESCL,
        JOB_OUTPUT_OPTIONS TYPE SSFCRESOP.

  DATA: WG_BUDAT_LOW  TYPE BKPF-BUDAT,
        WG_BUDAT_HIGH TYPE BKPF-BUDAT,
        WG_GJAHR      TYPE BKPF-GJAHR,
        WG_DIALOG.

  LS_OPTIONS-TDDEST   = 'LOCL'.


  WG_GJAHR = S_BUDAT-LOW(4).


"BREAK-POINT.


  IF S_BUDAT-HIGH IS NOT INITIAL
    AND S_BUDAT-HIGH(4) NE S_BUDAT-LOW(4).
    MESSAGE S000(Z01) DISPLAY LIKE 'E' WITH TEXT-M01.
    "'Preencher com o mesmo mês do período'.
  ELSE.
    VL_FORMNAME = 'ZLIBRO_MAYOR3'.
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        FORMNAME           = VL_FORMNAME
      IMPORTING
        FM_NAME            = VL_NAME
      EXCEPTIONS
        NO_FORM            = 1
        NO_FUNCTION_MODULE = 2
        OTHERS             = 3.


    IF WG_DIALOG EQ C_X.
      LS_CONTROL-NO_DIALOG = C_X.
*          LS_CONTROL-PREVIEW = C_X.
      MOVE-CORRESPONDING JOB_OUTPUT_OPTIONS TO LS_CONTROL.
      MOVE-CORRESPONDING JOB_OUTPUT_OPTIONS TO LS_OPTIONS.
      MOVE-CORRESPONDING JOB_OUTPUT_INFO TO LS_OPTIONS.
      MOVE JOB_OUTPUT_OPTIONS-TDPREVIEW TO LS_CONTROL-PREVIEW.

    ELSE.
      LS_CONTROL-NO_DIALOG = ' '.
*          LS_CONTROL-PREVIEW   = SPACE.
**  Impressora
*        LS_CONTROL-NO_DIALOG = ' '. "Evita la pantalla de opciones de salida del formulario
*          LS_OPTIONS-TDDEST   = ' '."'LOCL'.
*          LS_OPTIONS-TDIMMED  = ' '.
*          LS_OPTIONS-TDNEWID  = ' '.
*          LS_OPTIONS-TDNOARCH = ' '.
*
*          LS_CONTROL-DEVICE  = 'PRINTER'.
*          LS_CONTROL-GETOTF  = ' '.
    ENDIF.

    CLEAR:JOB_OUTPUT_INFO, JOB_OUTPUT_OPTIONS.

    CALL FUNCTION VL_NAME
      EXPORTING
        USER_SETTINGS      = ' '
        CONTROL_PARAMETERS = LS_CONTROL
        OUTPUT_OPTIONS     = LS_OPTIONS
        BUKRS              = S_BUKRS-LOW
        BUDAT_LOW          = S_BUDAT-LOW
        BUDAT_HIGH         = S_BUDAT-HIGH
        GJAHR              = WG_GJAHR
      IMPORTING
        JOB_OUTPUT_INFO    = JOB_OUTPUT_INFO
        JOB_OUTPUT_OPTIONS = JOB_OUTPUT_OPTIONS
      EXCEPTIONS
        FORMATTING_ERROR   = 1
        INTERNAL_ERROR     = 2
        SEND_ERROR         = 3
        USER_CANCELED      = 4
        OTHERS             = 5.

    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
    WG_DIALOG = C_X.
  ENDIF.
