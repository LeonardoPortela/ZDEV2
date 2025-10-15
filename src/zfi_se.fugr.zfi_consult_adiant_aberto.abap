FUNCTION ZFI_CONSULT_ADIANT_ABERTO.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LIFNR) TYPE  LIFNR
*"     REFERENCE(I_BURKS) TYPE  BUKRS
*"  EXPORTING
*"     REFERENCE(TG_ZFIE0206) TYPE  ZFIE0206_T
*"----------------------------------------------------------------------


CHECK I_BURKS is not INITIAL and I_LIFNR is not INITIAL.

  select *
  from bsik as a
  inner join bkpf as b
  on b~bukrs eq a~bukrs
  and b~belnr eq a~belnr
  and b~gjahr eq a~gjahr
  and b~blart ne 'VC'
  into CORRESPONDING FIELDS OF TABLE TG_ZFIE0206
  WHERE a~bukrs eq I_BURKS
    and a~lifnr eq I_LIFNR
    and a~bschl eq '29'.
 IF SY-SUBRC EQ 0.
 SELECT * FROM LFA1
 INTO TABLE @DATA(TG_LFA1)
 FOR ALL ENTRIES IN @TG_ZFIE0206
 WHERE LIFNR EQ @TG_ZFIE0206-LIFNR.
 IF SY-SUBRC EQ 0.
   LOOP AT TG_ZFIE0206 ASSIGNING FIELD-SYMBOL(<WS_ZFIE0206>).
     READ TABLE TG_LFA1 INTO DATA(WA_LFA1) WITH KEY LIFNR = <WS_ZFIE0206>-LIFNR.
     IF SY-SUBRC EQ 0.
     <WS_ZFIE0206>-NAME1 = WA_LFA1-NAME1.
     ENDIF.
   ENDLOOP.
 ENDIF.
 ENDIF.
ENDFUNCTION.
