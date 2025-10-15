"Name: \TY:CL_EAM_WORKREQ_TOHIERARCHY\ME:DETERMINE_HIER_FROM_IDENTLEV2\SE:END\EI
ENHANCEMENT 0 ZIMP_PM_LIMITE_LINESS.
if lv_level <= 100.
  select single low from tvarvc into @data(var_rv_page_size) where name eq 'ZIMP_PM_LIMIT_LINES_OBJ_TECN'. "Limite de linhas para exibir.
  if sy-subrc eq 0.
    lv_level = var_rv_page_size.
  endif.


  " Append to final table
  free: lt_hierarchy_level, ct_data, lt_hierarchy_authcheck, lt_hierarchy_no_auth, lt_hierarchy_base.

  check iv_techobj is not initial and
          iv_equiorfloc is not initial.

    append value #( technicalobject = iv_techobj
                    techobjisequiporfuncnlloc = iv_equiorfloc ) to lt_hierarchy_base.

    lo_hierachy = cl_eam_technicalobjhierarchy=>get_instance( ).

    " Get upper hierarchy
    lt_hierarchy = lt_hierarchy_base.
    lo_hierachy->determine_tohierarchy_up( changing ct_hierarchy = lt_hierarchy ).

    " Append to final table
    lt_hierarchy_level = corresponding #( lt_hierarchy ).
    clear lt_hierarchy.

    if iv_equiorfloc eq gc_floc.
      get_second_ident_level(
        exporting
          iv_floc      = conv #( iv_techobj )
        importing
          ev_ident_lev = lv_identifyinglevel2 ).
    endif.


  if lines( lt_hierarchy_level ) >=  lv_identifyinglevel2.
    lt_hierarchy = lt_hierarchy_base.
*    lv_level = 100 - lines( lt_hierarchy_level ).
    lo_hierachy->determine_tohierarchy_down( exporting iv_level     = lv_level
                                             changing  ct_hierarchy = lt_hierarchy ).
    lt_hierarchy_level = corresponding #( base ( lt_hierarchy_level ) lt_hierarchy ).
    clear lt_hierarchy.
  endif.

  sort lt_hierarchy_level by technicalobject techobjisequiporfuncnlloc ascending superiortechnicalobject superiorobjisequiporfuncnlloc descending.
  delete adjacent duplicates from lt_hierarchy_level comparing technicalobject techobjisequiporfuncnlloc.
  determine_levels( changing ct_hierarchy = lt_hierarchy_level ).

  " Fetch all other details about the technical object required
  " for the objects found in hierarchy
  ct_data = corresponding #( lt_hierarchy_level ).
  if_eam_workreq_tohierarchy~select_c_techobjcstmhiervh( exporting is_requested_elements  = is_requested_elements
                                                         changing  ct_c_techobjcstmhiervh = ct_data ).
  lt_hierarchy_authcheck = corresponding #( ct_data ).
  sort lt_hierarchy_authcheck by technicalobject techobjisequiporfuncnlloc.

*--------------------------------------------------------------------*
  " If the are some unauthorized entry,
  " check and do hierarchy reduction if required
*--------------------------------------------------------------------*
  if lines( lt_hierarchy_level ) ne lines( lt_hierarchy_authcheck ).

    loop at lt_hierarchy_level assigning <fs_hierarchy_level>.
      read table lt_hierarchy_authcheck transporting no fields binary search
        with key technicalobject = <fs_hierarchy_level>-technicalobject
                 techobjisequiporfuncnlloc = <fs_hierarchy_level>-techobjisequiporfuncnlloc.
      if sy-subrc ne 0.
        append <fs_hierarchy_level> to lt_hierarchy_no_auth.
      endif.
    endloop.

    lo_hierachy->process_hierarchy_reduction(
      exporting
        iv_technicalobject           = iv_techobj
        iv_techobjisequiporfuncnlloc = iv_equiorfloc
        it_not_allowed_objects       = lt_hierarchy_no_auth
      changing
        ct_hierarchy                 = lt_hierarchy_level ).

    clear ct_data.
    ct_data = corresponding #( lt_hierarchy_level ).

  endif.

*--------------------------------------------------------------------*
  " Fill ct_data with drilldownstate
*--------------------------------------------------------------------*
  adjust_hierlevel_drillstate(
    exporting
      it_hierarchy_level = lt_hierarchy_level
    changing
      ct_hierarchy       = ct_data ).
endif.
ENDENHANCEMENT.
