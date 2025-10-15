"Name: \PR:RIFLET20\FO:FILL_ADDITIONAL_FIELDS_F16\SE:END\EI
ENHANCEMENT 0 ZENHANCEMMENT_IW36.
** Inicio USER STORY 95865  / Anderson Oenning
   DATA: vg_equnr TYPE equnr,
         lv_imob  TYPE fleet-zzimobilizado.

   FIELD-SYMBOLS: <zzidade_eqpto>    TYPE any,
                  <zzidade_eqpto_>   TYPE any,
                  <equnr>            TYPE any,
                  <zzposition_cont>  TYPE any,
                  <zzposition_cont_> TYPE any,
                  <zzvida_util>      TYPE any,
                  <zzvida_util_>     TYPE any,
                  <fs_objnr>         TYPE any,
                  <fs_zzimob>        TYPE any.

   IF sy-tcode EQ 'IE36'.

     "Pegando valor da workearea.
     ASSIGN COMPONENT 'OBJNR' OF STRUCTURE cs_object
                                        TO <fs_objnr>.

     SELECT SINGLE zzimobilizado FROM fleet INTO lv_imob
       WHERE objnr = <fs_objnr>.

     IF sy-subrc IS INITIAL.

       "Pegando valor da workearea.
       ASSIGN COMPONENT 'ZZIMOB' OF STRUCTURE cs_object
                                          TO <fs_zzimob>.

       <fs_zzimob> =  lv_imob.

     ENDIF.

     "Pegando valor da workearea.
     ASSIGN COMPONENT 'ZZIDADE_EQPTO' OF STRUCTURE <ls_structure>
                                      TO <zzidade_eqpto>.

     "Pegando valor da workearea.
     ASSIGN COMPONENT 'ZZIDADE_EQPTO' OF STRUCTURE cs_object
                                      TO <zzidade_eqpto_>.

     IF <zzidade_eqpto> IS ASSIGNED.
       <zzidade_eqpto_> = <zzidade_eqpto>.
     ENDIF.
   ENDIF.


   "Pegando valor da workearea.
   ASSIGN COMPONENT 'ZZPOSITION_CONT' OF STRUCTURE <ls_structure>
                                    TO <zzposition_cont>.

   "Pegando valor da workearea.
   ASSIGN COMPONENT 'ZZPOSITION_CONT' OF STRUCTURE cs_object
                                    TO <zzposition_cont_>.


   IF <zzposition_cont> IS ASSIGNED.
     IF <zzposition_cont> IS NOT INITIAL.
       <zzposition_cont_> = <zzposition_cont>.
     ENDIF.
   ENDIF.

   "Pegando valor da workearea.
   ASSIGN COMPONENT 'ZZVIDA_UTIL' OF STRUCTURE <ls_structure>
                                    TO <zzvida_util>.

   "Pegando valor da workearea.
   ASSIGN COMPONENT 'ZZVIDA_UTIL' OF STRUCTURE cs_object
                                    TO <zzvida_util_>.


   IF <zzvida_util> IS ASSIGNED.
     IF <zzvida_util> IS NOT INITIAL.
       <zzvida_util_> = <zzvida_util>.
     ENDIF.
   ENDIF.


** Fim USER STORY 95865  / Anderson Oenning
ENDENHANCEMENT.
