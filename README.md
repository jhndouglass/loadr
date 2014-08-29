An R implementation of the Load Apportionment Model by Bowes et
al (2008). Takes vectors of concurrent pollutant concentration and flow
values to compute point and diffuse source components of pollutant
concentration.

DETAILS
The Load Apportionment Model attributes load at flow x between
point and diffuse sources based on the relationship between flow and
concentration. The model is described as

Co = A * Q^(B-1) + C * Q^(D-1)

Where Co is concentration, Q is discharge and A, B, C and D are parameters
to be estimated empirically.

Bowes MJ, Smith JT, Javie H, Neal C (2008). Modelling of
phosphorus inputs to rivers from diffuse and point sources. Science
of the Total Environment 395. pp125-138.
