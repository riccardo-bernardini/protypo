with Readable_Sequences.Generic_Sequences;

package Readable_Sequences.String_Sequences is
  new Generic_Sequences (Element_Type  => Character,
                         Element_Array => String);
