*&---------------------------------------------------------------------*
*&  Include           ZPMR0078_TOP
*&---------------------------------------------------------------------*

TABLES: equi,
        equz,
        iflot,
        crhd.

TYPES:
  BEGIN OF ty_crhd,
    arbpl      TYPE crhd-arbpl,
    werks      TYPE crhd-werks,
    ktext      TYPE crtx-ktext,
    begda      TYPE crhd-begda,
    aedat_grnd TYPE crhd-aedat_grnd,
    lvorm      TYPE crhd-lvorm,
    xsprr      TYPE crhd-xsprr,
  END OF ty_crhd.

DATA: t_equi TYPE TABLE OF equi,
      t_equz TYPE TABLE OF equz,
      t_iflo TYPE TABLE OF iflo,
      t_crhd TYPE TABLE OF ty_crhd,
      t_veiculos TYPE TABLE OF ztpm_m_veic_mobile.
