*&---------------------------------------------------------------------*
*&  Include           ZXCO1U21
*&---------------------------------------------------------------------*
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             REFERENCE(I_CAUFVD) LIKE  CAUFVD STRUCTURE  CAUFVD
*"             REFERENCE(I_AFPOD) LIKE  AFPOD STRUCTURE  AFPOD
*"----------------------------------------------------------------------

BREAK-POINT ID zpppi001.

MOVE-CORRESPONDING i_caufvd TO aufk.
