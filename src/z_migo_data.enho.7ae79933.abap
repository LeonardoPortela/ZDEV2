"Name: \PR:SAPLMIGO\TY:LCL_MIGO_KERNEL\ME:HEADER_GET\SE:END\EI
ENHANCEMENT 0 Z_MIGO_DATA.
*
  data: WA_ZMMT0066       type ZMMT0066,
        WL_SAB_DOM_FER    TYPE TABLE OF ISCAL_DAY,
        WA_SAB_DOM_FER    TYPE ISCAL_DAY.

  "Controle data de lançamento Fiscal
  DATA: VG_LAST_DAY  TYPE SY-DATUM,
        VG_FIRST_DAY TYPE SY-DATUM.

  FIELD-SYMBOLS: <fs_action> type ANY.

   ASSIGN ('(SAPLMIGO)GODYNPRO-ACTION') TO <fs_action>.
  IF <fs_action> IS ASSIGNED.

   IF <fs_action> NE 'A04'.
       IF goitem is INITIAL.
         clear GOHEAD-BLDAT.
       endif.
    ENDIF.
   ENDIF.
**************************************************************************************************************************
************* tratamento movto 833
 if goitem-bwart = '833'.
   clear: goitem-UMLGO, goitem-UMLGOBE, goitem-UMCHA.
 endif.
LOOP AT PT_GOITEM ASSIGNING FIELD-SYMBOL(<fs_item>).
  IF <fs_item>-BWART = '833'.
     clear: <fs_item>-UMLGO, <fs_item>-UMLGOBE, <fs_item>-UMCHA.
  ENDIF.

ENDLOOP.
**************************************************************************************************************************
** "MM-271216- CS2016000714-ALRS
*  if sy-UCOMM = 'OK_GO' and  goitem is INITIAL.
*    if GOHEAD-MBLNR is INITIAL and goitem is NOT INITIAL..
*      if GOHEAD-LFSNR is INITIAL and GOHEAD-BLDAT is not INITIAL.
*        MESSAGE E398(00) WITH 'Informe o numero da nota!'.
*      endif.
*    endif.
*  Endif.

*  if GOHEAD-MBLNR is INITIAL and GOHEAD-BUDAT is not INITIAL.
*    if GOHEAD-BUDAT ne sy-datum.
*      MESSAGE E398(00) WITH 'Data de Lançamento deve ser a Atual!'.
*    endif.
*     CONCATENATE SY-DATUM(6) '01' INTO VG_FIRST_DAY.
*     CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
*        EXPORTING
*          I_DATE = VG_FIRST_DAY
*        IMPORTING
*          E_DATE = VG_LAST_DAY.
*
*      REFRESH WL_SAB_DOM_FER.
*      CALL FUNCTION 'HOLIDAY_GET'
*        EXPORTING
*          FACTORY_CALENDAR           = 'ZF'
*          DATE_FROM                  = SY-DATUM
*          DATE_TO                    = VG_LAST_DAY
*        TABLES
*          HOLIDAYS                   = WL_SAB_DOM_FER
*        EXCEPTIONS
*          FACTORY_CALENDAR_NOT_FOUND = 1
*          HOLIDAY_CALENDAR_NOT_FOUND = 2
*          DATE_HAS_INVALID_FORMAT    = 3
*          DATE_INCONSISTENCY         = 4
*          OTHERS                     = 5.
*
*      READ TABLE WL_SAB_DOM_FER into WA_SAB_DOM_FER WITH KEY DATE = SY-DATUM.
*      IF SY-SUBRC = 0.
*        MESSAGE E398(00) WITH 'Lançamento não é dia útil'.
*      ELSE.
*        "Verifica ultimo dia útil
*        DO.
*          READ TABLE WL_SAB_DOM_FER into WA_SAB_DOM_FER WITH KEY DATE = VG_LAST_DAY.
*          IF SY-SUBRC NE 0.
*            EXIT.
*          ENDIF.
*          SUBTRACT 1 FROM VG_LAST_DAY.
*        ENDDO.
*        IF GOHEAD-BUDAT EQ VG_LAST_DAY.
*          SELECT SINGLE *
*            FROM ZMMT0066
*            INTO WA_ZMMT0066
*            WHERE USNAM = SY-UNAME.
*          IF SY-SUBRC NE 0.
*              MESSAGE E398(00) WITH 'Lançamento é ultimo dia útil, procurar Depto fiscal'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*  ENDIF.

ENDENHANCEMENT.
