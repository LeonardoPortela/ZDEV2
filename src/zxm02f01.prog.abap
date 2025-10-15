*----------------------------------------------------------------------*
***INCLUDE ZXM02F01 .
*----------------------------------------------------------------------*
FORM zf_status_0111.

  FIELD-SYMBOLS <fs_banfn> TYPE banfn.

  ASSIGN ('(SAPLMEGUI)MEREQ_TOPLINE-BANFN_EXT') TO <fs_banfn>.

  CHECK <fs_banfn> IS ASSIGNED.

  CHECK <fs_banfn> IS NOT INITIAL.

  SELECT SINGLE status_coupa id_coupa lifnr_coupa konnr_coupa FROM eban
    INTO (status_coupa,id_coupa,lifnr_coupa,konnr_coupa)
      WHERE banfn = <fs_banfn>.

ENDFORM.
