*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
class lcl_reference_converter definition.

  public section.
    types:
      ty_namespaces type range of namespace,
      ty_packages   type range of devclass.
    class-methods:
      create_instance
        returning
          value(result) type ref to lcl_reference_converter.
    methods:
      add_cust_code_refs_and_refappl
        importing
          references                         type if_ris_environment_types=>ty_t_senvi_tadir
          customer_obj                       type if_ycm_references=>ty_customer_object
          namespaces                         type ty_namespaces
          packages                           type ty_packages
          value(filter_refs_to_cust_objects) type abap_bool default abap_false
        changing
          custom_code_references             type if_ycm_references=>ty_object_refs.

endclass.

class lcl_reference_converter implementation.
  method create_instance.
    create object result.
  endmethod.

  method add_cust_code_refs_and_refappl.
    types: begin of ty_appl_component,
             object    type tadir-object,
             obj_name  type tadir-obj_name,
             devclass  type devclass,
             component type uffctr,
             namespace type tdevc-namespace,
             ps_posid  type ufps_posid,
           end of ty_appl_component.
    types:
      ty_tab_appl_components type standard table of ty_appl_component.

    types: begin of ty_relev_object_with_namesp.
        include type if_ycm_references=>ty_customer_object.
    types: namespace type tdevc-namespace,
           end of ty_relev_object_with_namesp.

    data:
      custom_code_reference type if_ycm_references=>ty_object_ref,
      appl_components       type ty_tab_appl_components,
      deviant_tadir_data    type if_ycm_references=>ty_customer_objects,
      relevant_cust_objects type standard table of ty_relev_object_with_namesp,
      relevant_sap_objects  type standard table of ty_relev_object_with_namesp.

    field-symbols:
      <reference>           like line of references,
      <appl_component>      type ty_appl_component,
      <deviant_tadir_entry> like line of deviant_tadir_data.

    if references is initial.
      return.
    endif.

    " get application components of ref_obj (SAP)
    select tadir~object tadir~obj_name tadir~devclass tdevc~component tdevc~namespace df14l~ps_posid from tadir
      inner join tdevc on tdevc~devclass = tadir~devclass
      inner join df14l on tdevc~component = df14l~fctr_id "#EC CI_BUFFJOIN
      into table appl_components
      for all entries in references where  tadir~pgmid = 'R3TR' and
                                           object      = references-ref_obj_type and
                                           obj_name    = references-ref_obj_name.

    sort appl_components by object obj_name.
    delete adjacent duplicates from appl_components.

    "ec filter findings: consider only selected namespaces for the customer objects (obj_type/obj_name)
    " we do not filter findings related to entered packages as we might lose too many findings;
    " depending on customer feedback we might add it again in the WHERE condition of SELECT
    select tadir~object tadir~obj_name tadir~devclass tadir~genflag tdevc~dlvunit tdevc~namespace from tadir
          inner join tdevc on tadir~devclass = tdevc~devclass "#EC CI_BUFFJOIN
          into corresponding fields of table relevant_cust_objects
          for all entries in references where
                                           tadir~pgmid    = 'R3TR' and
                                           " tadir~devclass  in packages and
                                           tdevc~namespace in namespaces and
                                           object      = references-obj_type and
                                           obj_name    = references-obj_name.

    "ec: filter out findings for used objects inside of entered namespaces or packages(ref_obj_type/ref_obj_name)
    if filter_refs_to_cust_objects = abap_true.
      select tadir~object tadir~obj_name tadir~devclass tadir~genflag tdevc~dlvunit tdevc~namespace from tadir
            inner join tdevc on tadir~devclass = tdevc~devclass "#EC CI_BUFFJOIN
            into corresponding fields of table relevant_sap_objects
            for all entries in references where
                                             tadir~pgmid = 'R3TR' and
                                             object      = references-ref_obj_type and
                                             obj_name    = references-ref_obj_name and
                                             not ( tadir~devclass in packages and tdevc~namespace in namespaces ).
    endif.
    " Cases like: (reference-object) ABAP include included into (customer_obj) ABAP report
    " ABAP include has its own TADIR entry. Maybe it belongs to another package ...
    select tadir~object tadir~obj_name tadir~devclass tadir~genflag tdevc~dlvunit from tadir
        inner join tdevc on tadir~devclass = tdevc~devclass "#EC CI_BUFFJOIN
        into table deviant_tadir_data
        for all entries in references where
          tadir~pgmid    = 'R3TR' and
          tadir~object   = references-obj_type and
          tadir~obj_name = references-obj_name and
          ( tadir~object <> customer_obj-object or tadir~obj_name <> customer_obj-obj_name ).

    loop at references assigning <reference>.
      if <reference>-obj_type is not initial and <reference>-obj_name is not initial.
        read table relevant_cust_objects with key object   = <reference>-obj_type
                                                  obj_name = <reference>-obj_name
                                         transporting no fields.
        if sy-subrc <> 0.
          continue.  " the finding (cust. obj.) is not within selected namespace(s), ignore it
        endif.
        " ec: used object and object that uses coexist within entered list of namespaces or packages;
        " ec: per parameter it is required to exclude this kind of findings
        if filter_refs_to_cust_objects = abap_true.
          read table relevant_sap_objects with key object   = <reference>-ref_obj_type
                                                   obj_name = <reference>-ref_obj_name
                                          transporting no fields.
          if sy-subrc <> 0.
            continue.  " the reference is inside selected namespace(s)/package(s), ignore it
          endif.
        endif.
      endif.
      clear custom_code_reference.
      move-corresponding <reference> to custom_code_reference.

      if custom_code_reference-obj_name <> customer_obj-obj_name
      or custom_code_reference-obj_type <> customer_obj-object.
        read table deviant_tadir_data
           assigning <deviant_tadir_entry> with key object   = <reference>-obj_type
                                                    obj_name = <reference>-obj_name.
        if sy-subrc = 0.
          custom_code_reference-devclass = <deviant_tadir_entry>-devclass.
          custom_code_reference-genflag  = <deviant_tadir_entry>-genflag.
          custom_code_reference-dlvunit  = <deviant_tadir_entry>-dlvunit.
        endif.
      endif.
      if custom_code_reference-devclass is initial.
        custom_code_reference-devclass = customer_obj-devclass.
        custom_code_reference-genflag  = customer_obj-genflag.
        custom_code_reference-dlvunit  = customer_obj-dlvunit.
      endif.

      read table appl_components assigning <appl_component> with key object = <reference>-ref_obj_type
                                                                   obj_name = <reference>-ref_obj_name.
      if sy-subrc = 0.
        custom_code_reference-ref_appl_component = <appl_component>-ps_posid.  " new requirement
      endif.

      " ec: referenced object(is supposed to belong SAP) and the object that uses (is supposed to belong customer)
      " have to be different; references inside same R3TR are not of interest
      if ( not ( <reference>-ref_obj_type = <reference>-obj_type and <reference>-ref_obj_name = <reference>-obj_name ) ).
        insert custom_code_reference into table custom_code_references.
      endif.
    endloop.

  endmethod.

endclass.
