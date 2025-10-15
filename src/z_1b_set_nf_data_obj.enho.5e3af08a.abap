"Name: \FU:J_1B_NF_OBJECT_UPDATE\SE:BEGIN\EI
ENHANCEMENT 0 Z_1B_SET_NF_DATA_OBJ.



*TYPES: BEGIN OF lit_item_change,
*        include TYPE  j_1bnflin,
*       selkz   type      j_1bdylin-selkz,
* END OF lit_item_change.

DATA: BEGIN OF lit_item_change OCCURS 0.

         INCLUDE STRUCTURE j_1bnflin.
        DATA:   selkz   LIKE      j_1bdylin-selkz.
       DATA: END OF lit_item_change.
"IF 1 = 2.
"
"DATA: it_item_change TYPE STANDARD TABLE OF  lit_item_change.
DATA: it_item   TYPE STANDARD TABLE OF  J_1BDYLIN.
" DATA: lit_item_change type standard table of   ZIT_J_1BNFLIN.
"     lwa_j_1bnfdoc   TYPE j_1bnfdoc.

DATA(lwa_j_1bnfdoc)     = obj_header.
  "it_item[] = OBJ_ITEM[].

 "it_item = CORRESPONDING #( OBJ_ITEM )
"data(lit_item_change) =   obj_item[] .
 it_item[]  = CORRESPONDING #( OBJ_ITEM[] ).
"lit_item_change[] = obj_item[] .




zcl_j_1bset_nf_data=>change_nf( CHANGING c_docheader = lwa_j_1bnfdoc
                                         c_it_item_obj   = it_item ).

obj_header   =  lwa_j_1bnfdoc.
obj_item[]  = CORRESPONDING #( it_item[] ).
"obj_item[]   =  it_item[].

" ENDIF.


ENDENHANCEMENT.
