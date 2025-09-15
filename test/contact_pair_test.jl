using Printf

surface_interaction_name = "cris"
line_end = @sprintf("\"%s\"", surface_interaction_name)


interaction = "steel on steel friction"
type = "SURFACE TO SURFACE"

lines = @sprintf "*Contact Pair, interaction=%s, type=%s" interaction type

line_end = @sprintf("*Contact Pair, interaction=\"%s\", type=%s", interaction, type)