FUNCTION-POOL zles_zmm0127_contingencia.    "MESSAGE-ID ..

DATA: l_destination         TYPE char40,
      l_zsdt0231            TYPE string,
      l_j_1bnfdoc           TYPE string,
      l_j_1bnflin           TYPE string,
      l_j_1bnfnad           TYPE string,
      l_j_1bnfstx           TYPE string,
      l_j_1bnfe_active      TYPE string,
      l_zib_nfe             TYPE string,
*
      lc_dados              TYPE zde_processo,
      lc_reg_criado         TYPE char01,
*
      t_zsdt0231_ecc        TYPE TABLE OF zsdt0231,
      t_j_1bnfdoc_ecc       TYPE TABLE OF j_1bnfdoc,
      t_j_1bnflin_ecc       TYPE TABLE OF j_1bnflin,
      t_j_1bnfnad_ecc       TYPE TABLE OF j_1bnfnad,
      t_j_1bnfstx_ecc       TYPE TABLE OF j_1bnfstx,
      t_j_1bnfe_active_ecc  TYPE TABLE OF j_1bnfe_active,
      t_zib_nfe_ecc         TYPE TABLE OF zib_nfe,
      t_j_1bnfftx_ecc       TYPE TABLE OF j_1bnfftx,
*
      w_zsdt0231_ecc        TYPE zsdt0231,
      w_j_1bnfdoc_ecc       TYPE j_1bnfdoc,
      w_j_1bnflin_ecc       TYPE j_1bnflin,
      w_j_1bnfnad_ecc       TYPE j_1bnfnad,
      w_j_1bnfstx_ecc       TYPE j_1bnfstx,
      w_j_1bnfe_active_ecc  TYPE j_1bnfe_active,
      w_zib_nfe_ecc         TYPE zib_nfe,
*
      t_ztnf_hana TYPE TABLE of ztnf_hana,
      t_j_1bnfdoc_hana      TYPE TABLE OF j_1bnfdoc,
      t_zsdt0231_hana       TYPE TABLE OF zsdt0231,
      t_j_1bnfe_active_hana TYPE TABLE OF j_1bnfe_active,
      t_zib_nfe_hana        TYPE TABLE OF zib_nfe,
      w_j_1bnfdoc_hana      TYPE j_1bnfdoc,
      w_zsdt0231_hana       TYPE zsdt0231,
      w_j_1bnfe_active_hana TYPE j_1bnfe_active,
      w_zib_nfe_hana        TYPE zib_nfe,

*
      w_layout              TYPE slis_layout_alv,
      t_fieldcat            TYPE slis_t_fieldcat_alv,
      t_alv                 TYPE TABLE OF zsdt0231,
      w_alv                 TYPE zsdt0231,
      l_importa             TYPE char01,
      l_grid_title          TYPE lvc_title,
      l_program             TYPE sy-repid.

* INCLUDE LZLES_ZMM0127_CONTINGENCIAD...     " Local class definition
