*&---------------------------------------------------------------------*
*&  Include           ZPMR0078_SCR
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_equnr FOR equi-equnr,
                s_eqtyp FOR equi-eqtyp,
                s_iwerk FOR equz-iwerk,
                s_erdat FOR equi-erdat,
                s_aedat FOR equi-aedat.

PARAMETERS: p_dt_mod TYPE c AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

SELECT-OPTIONS: s_tplnr  FOR iflot-tplnr,
                s_fltyp  FOR iflot-fltyp,
                s_iwerk2 FOR iflot-iwerk,
                s_erdat2 FOR iflot-erdat,
                s_aedat2 FOR iflot-aedat.

PARAMETERS: p_dtmod2 TYPE c AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b2.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.

SELECT-OPTIONS: s_arbpl FOR crhd-arbpl,
                s_werks FOR crhd-werks,
                s_begda FOR crhd-begda,
                s_aedat3 FOR crhd-aedat_grnd.

PARAMETERS: p_dtmod3 TYPE c AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b3.
