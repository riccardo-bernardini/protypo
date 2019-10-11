with Ada.Containers.Vectors;

with WPs;
with Partners;

package Projects is
   use type WPs.WP_Type;
   use type Partners.Partner_Type;

   package WP_Vectors is
         new Ada.Containers.Vectors (Index_Type   => Positive,
                                     Element_Type => WPs.WP_Type);

   package Partner_Vectors is
         new Ada.Containers.Vectors (Index_Type   => Positive,
                                     Element_Type => Partners.Partner_Type);

   type Project_Type is
      record
         Partner_List : Partner_Vectors.Vector;
         WP_List      : WP_Vectors.Vector;
      end record;
end Projects;
