*&---------------------------------------------------------------------*
*& Report  Z_ATUAL_LIPS2
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  Z_ATUAL_LIPS2.

DATA:  WA_LIPS TYPE LIPS.

PARAMETERS: P_VBELN TYPE LIPS-VBELN,
            P_POSNR TYPE LIPS-POSNR,
            P_VGBEL TYPE LIPS-VGBEL,
            P_VGPOS TYPE LIPS-VGPOS.

SELECT SINGLE *
  FROM LIPS
  INTO WA_LIPS
  WHERE VBELN = P_VBELN
  AND   POSNR = P_POSNR.

IF SY-SUBRC = 0.
  UPDATE LIPS SET VGBEL = P_VGBEL
                  VGPOS = P_VGPOS
    WHERE VBELN = P_VBELN
    AND   POSNR = P_POSNR.
  COMMIT WORK.
ENDIF.
