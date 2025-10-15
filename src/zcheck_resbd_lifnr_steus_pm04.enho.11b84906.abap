"Name: \PR:SAPLCOMD\FO:CHK_PURCH_DATA\SE:END\EI
ENHANCEMENT 0 ZCHECK_RESBD_LIFNR_STEUS_PM04.

*field-symbols: <afvgd>     type any,
*               <zvg_resb>  type any,
*               <zvg_steus> type any.



*data: wa_resb   type resb,
*      zva_steus type afvgd-steus.

IF sy-tcode EQ 'IW31' OR sy-tcode EQ 'IW32' OR sy-tcode EQ 'IW34'.
*  assign ('(SAPLCOBC)RESB_BT[]') to <zvg_resb>.
*  assign ('(SAPLCOIH)AFVGD-STEUS') to <zvg_steus>.
*  wa_resb = <zvg_resb>.
*  zva_steus = <zvg_steus>.

  IF afvgd-steus EQ 'PM04'.
    IF resbd-lifnr IS INITIAL.
      SET CURSOR FIELD 'RESBD-LIFNR'.
      MESSAGE e278(00) WITH 'Fornecedor para operações com chave PM04'.
    ENDIF.
  ENDIF.
ENDIF.

ENDENHANCEMENT.
