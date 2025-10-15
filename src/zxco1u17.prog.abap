*&---------------------------------------------------------------------*
*&  Include           ZXCO1U17
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_AUFNR) LIKE  CAUFV-AUFNR
*"       EXPORTING
*"             REFERENCE(E_COCI_AUFK) LIKE  COCI_AUFK
*"                             STRUCTURE  COCI_AUFK
*"----------------------------------------------------------------------

BREAK-POINT ID zpppi001.

MOVE-CORRESPONDING aufk TO e_coci_aufk.
