"Name: \PR:SAPLMRMH\EX:MRM_DRSEG_FILL_02\EI
ENHANCEMENT 0 Z_FOLHA_SERVICO.

*Corrigi atualizacao de SP21 folha de servico

  data: tl_essr type table of ESSR,
        WL_EK08 LIKE LINE OF T_EK08RN_TEMP,
        wl_tabix type sy-tabix.

  if sy-tcode eq 'ZLES0043'
  OR SY-TCODE EQ 'ZLES0040'
  OR SY-cprog EQ 'ZLESR0010'
  OR SY-cprog EQ 'ZLESR0014'
  OR SY-TCODE EQ 'ZMM0079'.

  if T_EK08RN_TEMP[] is not initial.
    select *
      from essr
       into table tl_essr
       for all entries in T_EK08RN_TEMP
        where LBLNI eq T_EK08RN_TEMP-lfbnr
          and loekz eq 'X'.

     if sy-subrc is initial.
       LOOP AT T_EK08RN_TEMP INTO WL_EK08.
         wl_tabix = sy-tabix.
         read table tl_essr
           with key lblni = wl_ek08-lfbnr transporting no fields.

         if sy-subrc is initial.
           move 'X' to wl_ek08-retpo.

           modify t_ek08rn_temp from wl_ek08 index wl_tabix.
         endif.
       ENDLOOP.
     endif.
  endif.
  endif.
ENDENHANCEMENT.
