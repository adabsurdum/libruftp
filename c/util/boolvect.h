
/**
  * Copyright (C) 2015  Roger Kramer
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  */

#ifndef _boolvect_h_
#define _boolvect_h_

struct boolean_vector {

	/**
	  * Returns the number of flags this vector can store.
	  */
	int (*cap)( struct boolean_vector * );

	/**
	  * Returns the number of *set* flags.
	  */
	int (*popcount)( struct boolean_vector * );

	bool (*setstate)( struct boolean_vector *, int b, bool state );

	/**
	  * Set (to '1')/clear (to '0') the specified flag and returns its
	  * previous value.
	  */
	bool (*set)( struct boolean_vector *, int b );
	bool (*clr)( struct boolean_vector *, int b );

	/**
	  * This method is the array equivalent of a stack.pop method.
	  * It iterates through all *set* flags; each call returns the
	  * zero-based index of the first set flag following whatever
	  * the previous call returned and then clears the flag.
	  * Iteration begins at zero and cycles through the array.
	  */
	int (*pop)( struct boolean_vector * );

	int (*write)( struct boolean_vector *, uint32_t *array, int n );

	void (*destroy)( struct boolean_vector * );
};

#endif

