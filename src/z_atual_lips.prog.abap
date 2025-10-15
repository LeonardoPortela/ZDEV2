*&---------------------------------------------------------------------*
*& Report  Z_ATUAL_LIPS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_ATUAL_LIPS.

DATA:  WA_EKKO TYPE EKKO.
DATA:  WA_EBAN TYPE EBAN.

PARAMETERS: P_EBELN TYPE EKKO-EBELN,
            P_KALSM TYPE EKKO-KALSM.


PARAMETERS: P_BANFN TYPE EBAN-BANFN,
            P_FRGST TYPE EBAN-FRGST.


IF P_EBELN IS NOT INITIAL.
  SELECT SINGLE *
    FROM EKKO
    INTO WA_EKKO
    WHERE EBELN = P_EBELN.

  IF SY-SUBRC = 0.
    UPDATE EKKO SET KALSM = P_KALSM
      WHERE EBELN = P_EBELN.
    COMMIT WORK.
  ENDIF.
ENDIF.

IF P_BANFN IS NOT INITIAL.
  SELECT SINGLE *
    FROM EBAN
    INTO WA_EBAN
    WHERE BANFN = P_BANFN.

  IF SY-SUBRC = 0.
    UPDATE EBAN SET  FRGST = P_FRGST
       WHERE BANFN = P_BANFN.
    COMMIT WORK.
  ENDIF.
ENDIF.
